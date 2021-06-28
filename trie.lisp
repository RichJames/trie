;;;;
;;;;  Experiments with using tries
;;;;     

(defpackage #:trie
  (:use #:cl :cl-ppcre))

(in-package #:trie)

(defparameter *phrases* (list
                         (list 1 "forward shaft lean")
                         (list 2 "hands leading the club")
                         (list 3 "good contact")
                         (list 11 "good compression") ; not expecting to find this
                         (list 4 "setup position") ; not expecting to find this
                         (list 5 "early extension")
                         (list 6 "C programming")
                         (list 7 "project manager")
                         (list 8 "FORTRAN programmer") ;not expecting to find this
                         (list 9 "Common Lisp")
                         (list 10 "Common Lisp programmer")))

(defstruct (tree-node (:conc-name nil))
  key
  phrase-id
  children)

(defparameter *trie* (build-trie))

(defun build-trie ()
  (loop :with root = (make-tree-node :key "")
        :for phrase :in *phrases*
        :while phrase
        :do (trie-insert phrase root)
        :finally (return root)))

(defun trie-insert (phrase trie)
  (labels ((insert (id words root)
             (let ((node (dfs-find (first words) root)))
               (cond ((null words)
                      (setf (phrase-id root) (cons id (phrase-id root)))
                      root)
                     
                     ((null node) (insert id (cdr words) (add-child root (first words))))

                     (t (insert id (cdr words) node))))))

    (insert (first phrase) (ppcre:all-matches-as-strings "([\\S]+)" (second phrase)) trie)))

(defun add-child (node key)
  (let ((child (make-tree-node :key key)))
    (setf (children node) (cons child (children node)))
    child))

(defun dfs-trie (fn root)
  "Depth-first apply fn to each node of the tree."
  (funcall fn (key root))
  (dolist (child (children root))
    (dfs-trie fn child)))

(defun dfs-find (word root)
  "Depth-first search the tree for word."
  (cond ((string= word (key root)) root)
        (t (dolist (child (children root))
             (if (dfs-find word child)
                 (return child))))))

(defun find-matches (trie file)
  (labels ((rec (stream word node acc)
             (if (null word)
                 acc
                 (let ((find-node (find-if #'(lambda (x) (string= (key x) word)) (children node))))
                   
                   (cond ((and (null find-node) (equal node trie)) (rec stream
                                                                        (get-word stream)
                                                                        trie
                                                                        acc))

                         ((null find-node)                         (rec stream
                                                                        word
                                                                        trie
                                                                        acc))

                         ((null (children find-node))              (rec stream
                                                                        (get-word stream)
                                                                        trie
                                                                        (if (phrase-id find-node)
                                                                            (append (phrase-id find-node) acc)
                                                                            acc)))

                         (t                                        (rec stream
                                                                        (get-word stream)
                                                                        find-node
                                                                        (if (phrase-id find-node)
                                                                            (append (phrase-id find-node) acc)
                                                                            acc))))))))
    
    (with-open-file (stream file)
      (report-results (rec stream (get-word stream) trie (list))))))

(defun report-results (id-list)
  (let ((sorted-id-list (sort id-list #'<)))

    (loop :for result :in (remove-duplicates (mapcar #'(lambda (x) (list x (count x sorted-id-list))) sorted-id-list) :key 'car)
          :for (phrase-id phrase-count) = result
          :do (format t "~%Found '~a' ~d times" (second (find phrase-id *phrases* :key 'car)) phrase-count))))


(let ((words)
      (word))
  (defun get-word (stream)
    (cond (words
           (setf word (car words)
                 words (cdr words))
           (if word
               (string-downcase word)
               (get-word stream)))

          (t (setf words (read-line stream nil nil))
             (cond ((null words) nil)
                   (t (setf words (ppcre:all-matches-as-strings "([a-zA-Z]+)" words))
                      (get-word stream))))))

  (defun reset-words-buffers ()
    (setf words nil
          word  nil)))

(defun test-get-words (file)
  (reset-words-buffers)
  (with-open-file (stream file)

    (loop :for word = (get-word stream)
          :while word
          :do (format t "~%~a" word))))
