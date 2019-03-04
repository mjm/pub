FROM node:10

LABEL "com.github.actions.name"="Elm"
LABEL "com.github.actions.description"="Use elm commands in GitHub actions"
LABEL "com.github.actions.icon"="paperclip"
LABEL "com.github.actions.color"="orange"

LABEL "repository"="http://github.com/mjm/pub"
LABEL "maintainer"="Matt Moriarity <matt@mattmoriarity.com>"

RUN npm install -g elm elm-test --unsafe-perm=true

ENTRYPOINT ["elm"]
CMD []
