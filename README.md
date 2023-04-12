# Vicer

Vicer is a MirageOS unikernel which serves our blog content from a Git repository and provides dynamic features such as article comments. Vicer is heavily inspired by [unipi](https://github.com/roburio/unipi/).

## Configuration

```bash
mirage configure -t <target> \
  --port # The TCP port to listen on (defaults to 1965)
  --hook "xxx" \ # Secret URL of the webhook to resynchronize content repository state with remote (beginning "/" must not be specified)
  --certs-remote "git@localhost:certificates.git#master" \ # TLS certificates git remote
  --blog-remote "git@localhost:blog.git#master" \ # Blog content git remote
  --comments-remote "git@localhost:comments.git#master" \ # Comments store git remote
  --ssh-key "xxx" \ # SSH private key
  --ssh-authenticator "SHA256:xxx" # Authenticate the SSH remote
```
