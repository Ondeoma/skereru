<script lang="scala">
import skereru.web.*
import skereru.shared.*

import java.util.UUID
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.util.{Failure, Success}

// skawau（ファイル共有）のファイル・フォルダを選ぶ画面（モーダル）.
//  - folderMode = false: ファイルを選んで onAttach（添付する）か onLink（リンクを入れる. フォルダも選べる）.
//  - folderMode = true: 保存先のフォルダを選んで onFolder（None は既定の「メールの添付」）.
case class Props(folderMode: Boolean = false,
                 onAttach: List[SkawauItemDto] => Unit = _ => (),
                 onLink: List[SkawauItemDto] => Unit = _ => (),
                 onFolder: Option[UUID] => Unit = _ => (),
                 onClose: Unit => Unit)

case class State(folder: RV[Option[SkawauFolderDto]] = RV(None),
                 q: RV[String] = RV(""),
                 searching: RV[Boolean] = RV(false),
                 shown: RV[List[SkawauItemDto]] = RV(Nil),
                 picked: RV[List[SkawauItemDto]] = RV(Nil),
                 error: RV[String] = RV(""))

def initState(props: Props) = State()

def mounted(s: State, props: Props): Unit = open(s, None)

def open(s: State, id: Option[UUID]): Unit =
  Api.get[SkawauFolderDto]("/api/integrations/skawau/browse" + id.fold("")(i => s"?folder=$i")).onComplete {
    case Success(f) => s.folder := Some(f); s.shown := f.items; s.searching := false; s.error := ""
    case Failure(e) => s.error := Option(e.getMessage).getOrElse("skawau を読めませんでした")
  }

def search(s: State): Unit =
  if (s.q.v.trim.isEmpty) back(s)
  else Api.get[List[SkawauItemDto]]("/api/integrations/skawau/search?q=" + scala.scalajs.js.URIUtils.encodeURIComponent(s.q.v.trim)).onComplete {
    case Success(l) => s.shown := l; s.searching := true
    case Failure(e) => s.error := Option(e.getMessage).getOrElse("探せませんでした")
  }

/** 検索をやめて、今のフォルダの中身に戻る. */
def back(s: State): Unit = { s.searching := false; s.shown := s.folder.v.toList.flatMap(_.items) }

/** 出す一覧（検索中は検索の結果）. 保存先を選ぶときはフォルダだけ. */
def items(s: State, props: Props) = s.shown.mapC(_.filter(i => !props.folderMode || i.isDir))
def crumbs(s: State) = s.folder.mapC(_.toList.flatMap(f => f.path))
def here(s: State) = s.folder.mapC(_.map(_.name).getOrElse(""))
def searching(s: State) = s.searching

def click(s: State, props: Props, i: SkawauItemDto): Unit =
  if (i.isDir) { s.q := ""; open(s, Some(i.id)) }
  else if (!props.folderMode) toggle(s, i)

def toggle(s: State, i: SkawauItemDto): Unit =
  s.picked := (if (s.picked.v.exists(_.id == i.id)) s.picked.v.filterNot(_.id == i.id) else s.picked.v :+ i)
def isPicked(s: State, i: SkawauItemDto) = s.picked.mapC(_.exists(_.id == i.id))
def pickedCount(s: State) = s.picked.mapC(_.size)
def nonePicked(s: State) = s.picked.mapC(_.isEmpty)
/** 添付できない（フォルダを選んでいる）. */
def cannotAttach(s: State) = s.picked.mapC(p => p.isEmpty || p.exists(_.isDir))
def icon(i: SkawauItemDto): String = if (i.isDir) "📁" else "📄"
def size(i: SkawauItemDto): String = if (i.isDir) "" else Fmt.bytes(i.size)

def attach(s: State, props: Props): Unit = { props.onAttach(s.picked.v); props.onClose(()) }
def link(s: State, props: Props): Unit = { props.onLink(s.picked.v); props.onClose(()) }
def saveHere(s: State, props: Props): Unit = { props.onFolder(s.folder.v.map(_.id)); props.onClose(()) }
def saveDefault(props: Props): Unit = { props.onFolder(None); props.onClose(()) }
def title(props: Props): String = s"${AppNames.nameOf("skawau")}" + (if (props.folderMode) " の保存先" else " から選ぶ")
</script>

<template>
  <Modal :onClose="props.onClose" :wide="true">
    <h2>{{ title(props) }}</h2>
    <p v-if="s.error.mapC(_.nonEmpty)" class="err small">{{ s.error }}（設定の「連携」で連携しているか確かめてください）</p>
    <form v-if="!props.folderMode" class="row" @submit.prevent="search(s)">
      <input type="search" v-model="s.q" placeholder="名前で探す" class="grow">
      <button type="submit" class="small">探す</button>
    </form>
    <nav v-if="searching(s).mapC(!_)" class="crumbs small">
      <template v-for="c in crumbs(s)"><a href="#" @click.prevent="open(s, Some(c.id))">{{ c.name }}</a> / </template>
      <b>{{ here(s) }}</b>
    </nav>
    <p v-if="searching(s)" class="small"><a href="#" @click.prevent="back(s)">← フォルダに戻る</a></p>
    <div class="list">
      <div v-for="i in items(s, props)" :key="i.id" class="item" @click="click(s, props, i)">
        <input v-if="!props.folderMode" type="checkbox" :checked="isPicked(s, i)" @click.stop="toggle(s, i)">
        <span>{{ icon(i) }}</span><span class="grow name">{{ i.name }}</span><span class="muted small">{{ size(i) }}</span>
      </div>
      <p v-if="items(s, props).mapC(_.isEmpty)" class="muted small">{{ if (props.folderMode) "（フォルダはありません）" else "（何もありません）" }}</p>
    </div>
    <div class="actions">
      <button @click="props.onClose(())">キャンセル</button>
      <template v-if="!props.folderMode">
        <span class="muted small">{{ pickedCount(s) }} 件</span>
        <button :disabled="nonePicked(s)" title="相手は skawau の権限で開きます（共有していないと開けません）" @click="link(s, props)">リンクを入れる</button>
        <button class="primary" :disabled="cannotAttach(s)" @click="attach(s, props)">添付する</button>
      </template>
      <template v-if="props.folderMode">
        <button @click="saveDefault(props)">既定（メールの添付）に保存</button>
        <button class="primary" :disabled="s.folder.mapC(_.isEmpty)" @click="saveHere(s, props)">このフォルダに保存</button>
      </template>
    </div>
  </Modal>
</template>

<style scoped>
.crumbs { margin: .4rem 0; }
.list { max-height: 50vh; overflow: auto; border: 1px solid var(--line); border-radius: 8px; }
.item { display: flex; align-items: center; gap: .5rem; padding: .45rem .7rem; border-bottom: 1px solid var(--line); cursor: pointer; }
.item:hover { background: var(--accent-weak); }
.item:last-child { border-bottom: none; }
.name { word-break: break-all; }
.err { color: var(--danger); }
.list p { padding: .6rem; margin: 0; }
</style>
