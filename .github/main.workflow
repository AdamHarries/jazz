workflow "Deploy books" {
  resolves = ["Upload to Release"]
  on = "push"
}
