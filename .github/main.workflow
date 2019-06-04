workflow "Deploy books" {
  resolves = ["Upload to Release"]
  on = "push"
}

action "Upload to Release" {
  uses = "JasonEtco/upload-to-release@v0.1.1"
  secrets = ["GITHUB_TOKEN"]
  args = "books/bb.pdf"
}
