
if test -d /usr/local/go
  set -x PATH $PATH /usr/local/go/bin
end

if test -d ~/Go
  set -x GOPATH ~/Go
  set -x PATH $PATH $GOPATH/bin
end

