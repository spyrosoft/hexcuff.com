(defvar *this-file* #.(or *compile-file-pathname* *load-truename*))

(load "utilities/routing.lisp")

(route-directory "/cuffs/" "/pages/cuffs" "text/html")
(route-file "/about-us/" "/pages" "about-us" "html" "text/html")
(route-file "/contact-us/" "/pages" "contact-us" "html" "text/html")
(route-file "/cuff-size/" "/pages" "cuff-size" "html" "text/html")

;; Route / to index.html and serve it as text/html
(route-file "/" "/pages" "index" "html" "text/html")

;; Route everything in /css/ to the /css directory and serve it as text/html
(route-directory "/css/" "/css" "text/css")

;; Route everything in /js/ to the /js directory and serve it as text/javascript
(route-directory "/js/" "/js" "text/javascript")

;; Route everything in /images/ to the /images directory and serve it as whatever mime type is appropriate
(route-directory "/images/" "/images")

;; Route /robots.txt to the actual file and serve it as text/plain
(route-file "/robots.txt" "/" "robots" "txt" "text/plain")