package skereru.mail

/** SMTP の待ち受けの種類。Mx は外からの受信（25番）、Submission はメールソフトからの送信（587番・465番。認証が必須）。 */
enum SmtpMode {
  case Mx, Submission
}
