(require 'esh-maint)
(require 'blorg)
(setq blorg-put-full-post '(post feed))
(setq blorg-previous-posts-number 20)
(setq blorg-parg-in-headlines 3)
(setq blorg-put-author-in-post nil)
(setq blorg-post-number-per-page
      '((index . 10) (feed . 10) (tag . 10) (month . 10)))
;; blorg-publish needs this:
;;(setq org-hide-leading-stars nil)
;;(require 'fast-lock)
;;(setq font-lock-support-mode 'fast-lock-mode)
;;(setq font-lock-support-mode 'jit-lock-mode)

(defun blorg-insert-ga ()
  (insert "<script type=\"text/javascript\">
var gaJsHost = ((\"https:\" == document.location.protocol) ? \"https://ssl.\" : \"http://www.\");
document.write(unescape(\"%3Cscript src='\" + gaJsHost + \"google-analytics.com/ga.js' type='text/javascript'%3E%3C/script%3E\"));
</script>
<script type=\"text/javascript\">
try {
var pageTracker = _gat._getTracker(\"UA-9661548-1\");
pageTracker._trackPageview();
} catch(err) {}</script>"))

(defmacro blorg-side-menu ()
  `(progn
     (insert "<div id=\"sidemenu\">
<div id=\"blog-author\">
  <h3>Me</h3>
  <ul>
    <li><a href=\"")
     (blorg-insert-mailto-email)
     (insert "\">email</a></li>
    <li><a href=\"mega.gpg.asc\">gpg key</a></li>
    <li><a href=\"cv/cv-eng.pdf\">cv (english)</a></li>
    <li><a href=\"cv/cv-hun.pdf\">cv (hungarian)</a></li>
    <li><a href=\"https://github.com/melisgl\">git</a></li>
  </ul>
</div>
<div id=\"tags\">
  <h3>Tags</h3>")
     (blorg-insert-tags-as-cloud)
  (insert "</div>")
  (insert "<div><h3>Archive</h3>")
  (blorg-insert-archives)
  (insert "</div>")
  (insert "</div>")
  (insert "<div id=\"rightmenu\">")
  (when (boundp 'blorg-feed-url)
    (insert "<div id=\"feed\"><a href=\"")
    (insert blorg-feed-url)
    (insert "\"><img border=0 src=\"images/feed-icon-14x14.png\">&nbsp;atom feed</a></div>"))
  (insert "<div id=\"older-posts\"><h3>Older posts</h3>")
  (blorg-insert-previous-posts)
  (insert "</div>")
  (insert "</div>")))

(setq blorg-index-template
      "
<body>
<div id=\"content\">

<div id=\"blog-title\">
  <h1><a href=\"(blorg-insert-index-url)\">(blorg-insert-page-title)</a></h1>
</div>

(blorg-side-menu)

<div id=\"main\">
(blorg-insert-content)
</div>

</div>
(blorg-insert-ga)
</body>")

(setq blorg-month-page-template
"
<body>
<div id=\"content\">

<div id=\"blog-title\">
  <h1><a href=\"(blorg-insert-index-url)\">(blorg-insert-page-title) - (blorg-insert-month-name)</a></h1>
</div>

(blorg-side-menu)

<div id=\"main\">
(blorg-insert-content)
</div>

</div>
(blorg-insert-ga)
</body>")

(setq blorg-tag-page-template "
<body>
<div id=\"content\">

<div id=\"blog-title\">
  <h1><a href=\"(blorg-insert-index-url)\">(blorg-insert-page-title) - (blorg-insert-tag-name)</a></h1>
</div>

(blorg-side-menu)

<div id=\"main\">
(blorg-insert-content)
</div>

</div>
(blorg-insert-ga)
</body>")

(setq blorg-post-page-template "
<body>
<div id=\"content\">

<div id=\"blog-title\">
  <h1><a href=\"(blorg-insert-index-url)\">(blorg-insert-page-title)</a></h1>
</div>

(blorg-insert-content)

</div>
(blorg-insert-ga)
</body>")
