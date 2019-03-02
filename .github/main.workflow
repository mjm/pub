workflow "Deploy to production" {
  on = "push"
  resolves = ["Deploy app to S3"]
}

action "Install dependencies" {
  uses = "actions/npm@master"
  args = "ci"
}

action "Build app" {
  uses = "actions/npm@master"
  needs = ["Install dependencies"]
  args = "run build"
}

action "Deploy app to S3" {
  uses = "actions/aws/cli@master"
  needs = ["Build app"]
  args = "s3 cp dist/ s3://pub.mattmoriarity.com/ --recursive --acl public-read"
  secrets = ["AWS_ACCESS_KEY_ID", "AWS_SECRET_ACCESS_KEY"]
}
