package skereru.web

import io.github.ondeoma.scalactive.components.ComponentManager.*

object Dialog {
  val current: RV[Option[DialogReq]] = RV(None)
  private def open(r: DialogReq): Unit = current := Some(r)
  def confirm(message: String, ok: String = "OK", danger: Boolean = true)(f: => Unit): Unit =
    open(DialogReq(message, ok, danger, None, Nil, cancel = true, _ => f, () => ()))
  def prompt(message: String, default: String = "", ok: String = "OK")(f: String => Unit): Unit =
    open(DialogReq(message, ok, danger = false, Some(RV(default)), Nil, cancel = true, f, () => ()))
  def choose(message: String,
             choices: List[(String, String)],
             ok: String = "OK",
             danger: Boolean = false)(f: String => Unit): Unit =
    open(DialogReq(
      message,
      ok,
      danger,
      Some(RV(choices.headOption.map(_._1).getOrElse(""))),
      choices,
      cancel = true,
      f,
      () => ()
    ))
  def alert(message: String): Unit =
    open(DialogReq(message, "OK", danger = false, None, Nil, cancel = false, _ => (), () => ()))
  def close(ok: Boolean): Unit = current.v.foreach { r =>
    current := None
    if (ok) r.onOk(r.input.map(_.v).getOrElse("")) else r.onCancel()
  }
}
