;;;;
;;;;  Experiments with using tries
;;;;     

(defpackage #:trie
  (:use #:cl :cl-ppcre))

(in-package #:trie)

(defstruct (tree-node (:conc-name nil))
  key
  phrase-id
  children)

(defparameter *phrases* (list))

(defun build-trie (file)
  (setf *phrases* (list))
  (with-open-file (stream file)
    (loop :with root = (make-tree-node :key "")
          :for phrase = (read-line stream nil nil)
          :for phrase-id :upfrom 1
          :while phrase
          :do (progn
                (setf *phrases* (append (list (list phrase-id phrase)) *phrases*))
                (trie-insert (list phrase-id phrase) root))
          :finally (return root))))

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
  (let ((words)
        (word))
    (with-open-file (stream file)
      (labels ((get-word ()
                 (cond (words
                        (setf word (car words)
                              words (cdr words))
                        (if word
                            (string-downcase word)
                            (get-word)))

                       (t (setf words (read-line stream nil nil))
                          (cond ((null words) nil)
                                (t (setf words (ppcre:all-matches-as-strings "([a-zA-Z]+)" words))
                                   (get-word))))))
               
               (rec (word node acc)
                 (if (null word)
                     acc
                     (let ((find-node (find-if #'(lambda (x) (string= (key x) word)) (children node))))
                       
                       (cond ((and (null find-node) (equal node trie)) (rec (get-word)
                                                                            trie
                                                                            acc))

                             ((null find-node)                         (rec word
                                                                            trie
                                                                            acc))

                             ((null (children find-node))              (rec (get-word)
                                                                            trie
                                                                            (if (phrase-id find-node)
                                                                                (append (phrase-id find-node) acc)
                                                                                acc)))

                             (t                                        (rec (get-word)
                                                                            find-node
                                                                            (if (phrase-id find-node)
                                                                                (append (phrase-id find-node) acc)
                                                                                acc))))))))
        
        (report-results (rec (get-word) trie (list)))))))

(defun report-results (id-list)
  (let ((sorted-id-list (sort id-list #'<)))

    (loop :for result :in (remove-duplicates (mapcar #'(lambda (x) (list x (count x sorted-id-list))) sorted-id-list) :key 'car)
          :for (phrase-id phrase-count) = result
          :do (format t "~%Found '~a' ~d times" (second (find phrase-id *phrases* :key 'car)) phrase-count))))

(defun find-phrases (phrase-file file-to-search)
  (let ((trie (build-trie phrase-file)))
    (find-matches trie file-to-search)))
