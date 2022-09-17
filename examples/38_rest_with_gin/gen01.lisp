
(let* ((idx 1)
       (name "curl_postAlbums")
       (bash-fn  (format nil "source00/setup~2,'0d_~a.sh" idx name)))
  (with-open-file (s bash-fn
		     :if-exists :supersede
		     :direction :output
		     :if-does-not-exist :create)
    (flet ((out (&rest rest)
	     (format s "~{~a~^ \\~%~}" rest))
	   )
      (out "curl"
	   "http://localhost:8080/albums"
	   "--include"
	   "--header \"Content-Type: application/json\""
	   "--request \"POST\""
	   "--data '{\"id\":\"4\",\"title\":\"The Modern Sound of Betty Carter\",\"artist\":\"Betty Carter\",\"price\":49.99}'")))
  ;;pip3 install --user beautysh
  (sb-ext:run-program "/home/martin/.local/bin/beautysh"
		      `(,bash-fn)))

(let* ((idx 2)
       (name "curl_getAlbums")
       (bash-fn  (format nil "source00/setup~2,'0d_~a.sh" idx name)))
  (with-open-file (s bash-fn
		     :if-exists :supersede
		     :direction :output
		     :if-does-not-exist :create)
    (flet ((out (&rest rest)
	     (format s "~{~a~^ \\~%~}" rest))
	   )
      (out "curl"
	   "http://localhost:8080/albums"
	   "--header \"Content-Type: application/json\""
	   "--request \"GET\"")))
  ;;pip3 install --user beautysh
  (sb-ext:run-program "/home/martin/.local/bin/beautysh"
		      `(,bash-fn)))
