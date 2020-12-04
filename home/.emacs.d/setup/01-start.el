(defvar dmj-org-capture-templates
      '(("r" "Reference" entry (file "reference.org")
         "* %? %^g" :prepend t)
        ;; ("t" "Todo Inbox" entry (file+headline "" "Todos")
        ;;  "* TODO %?\n  Added %u\n  %i" :prepend t)
        ("b" "Bookmark" entry (file "bookmarks.org")
         "* %?\n %I")
        ;; http://members.optusnet.com.au/~charles57/GTD/datetree.html
        ("n" "Notes inbox" entry (file+headline "log.org" "Inbox")
         "* %^{Description} %^g %?
Added: %U")
        ("t" "make a task" entry (file+datetree "log.org")
         "* TODO %^{Description} %^g
 %?
Added: %U")))

;; Try to load local settings ahead of time
(require 'init-local-preload nil t)
