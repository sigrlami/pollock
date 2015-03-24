import Snap

site = writeText "Hello, Pollock!"

main = quickHttpServe site
