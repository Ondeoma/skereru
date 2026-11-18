<script lang="scala">
import skereru.web.*

// ユーザーのアイコン（画像がなければ名前の1文字目）. picture は保存したアイコンか IdP（sruru など）の画像
case class Props(name: String, picture: Option[String] = None, size: Int = 32)

def style(props: Props): String =
  s"background: hsl(${Fmt.hue(props.name)} 55% 45%); width: ${props.size}px; height: ${props.size}px; font-size: ${props.size * 0.45}px"

// 画像が読めない（IdP でアイコンを消した・URL が古いなど）ときは、名前の1文字目に戻す
case class State(failed: RV[Boolean] = RV(false))
def initState() = State()
def showImg(s: State, props: Props) = s.failed.mapC(f => props.picture.isDefined && !f)
</script>

<template>
  <span class="avatar" :style="style(props)">
    <img v-if="showImg(s, props)" :src='props.picture.getOrElse("")' alt="" referrerpolicy="no-referrer" @error="s.failed := true">
    <template v-else>{{ Fmt.initials(props.name) }}</template>
  </span>
</template>

<style scoped>
.avatar { border-radius: 50%; color: #fff; display: inline-flex; align-items: center; justify-content: center; font-weight: 700; flex: none; overflow: hidden; }
.avatar img { width: 100%; height: 100%; object-fit: cover; display: block; }
</style>
