workflow "Deploy Site" {
  on = "push"
  resolves = [
    "maxheld83/rsync@v0.1.1",
    "actions/bin/filter@master",
  ]
}

action "actions/bin/filter@master" {
  uses = "actions/bin/filter@master"
  args = "ref refs/heads/master"
}

action "maxheld83/rsync@v0.1.1" {
  uses = "maxheld83/rsync@v0.1.1"
  needs = ["actions/bin/filter@master"]
  secrets = ["SSH_PRIVATE_KEY", "SSH_PUBLIC_KEY"]
  env = {
    HOST_NAME = "ebsf.uk"
    HOST_IP = "178.62.111.11"
    HOST_FINGERPRINT = "ebsf.uk ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBMDu6IFy9fYsqP45aaJAeHWMYhJxbZXGWm0jILr9T8JO8L6bg1zLeKmVEDQ3e+bCLp3LOoj+uV7v1iuN3gMCfDg="
  }
  args = ["--inplace", "--no-p", "--no-g", "--chown=deploybot:www-data", "$GITHUB_WORKSPACE/site/*", "deploybot@$HOST_NAME:/var/www/ebsf.uk/"]
}
