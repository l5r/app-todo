(in-package :mu-cl-resources)

(setf *include-count-in-paginated-responses* t)
(setf *supply-cache-headers-p* t)
(setf sparql:*experimental-no-application-graph-for-sudo-select-queries* t)
(setf *cache-model-properties-p* t)
(setf mu-support::*use-custom-boolean-type-p* nil)
(setq *cache-count-queries-p* t)
(setf sparql:*query-log-types* nil) ;; hint: use app-http-logger for logging queries instead, all is '(:default :update-group :update :query :ask)


;; example
;; (define-resource dataset ()
;;   :class (s-prefix "dcat:Dataset")
;;   :properties `((:title :string ,(s-prefix "dct:title"))
;;                 (:description :string ,(s-prefix "dct:description")))
;;   :has-one `((catalog :via ,(s-prefix "dcat:dataset")
;;                       :inverse t
;;                       :as "catalog"))
;;   :has-many `((theme :via ,(s-prefix "dcat:theme")
;;                      :as "themes"))
;;   :resource-base (s-url "http://webcat.tmp.semte.ch/datasets/")
;;   :on-path "datasets")

;; reading in the domain.json
(read-domain-file "domain.json")

(define-resource todo-item ()
  :class (s-prefix "todo:TodoItem")
  :properties `((:title :string ,(s-prefix "todo:title"))
                (:completed-at :datetime,(s-prefix "todo:completedAt"))
                (:deadline :datetime ,(s-prefix "todo:deadline")))
  :has-one `((todo-list :via ,(s-prefix "todo:items")
                        :inverse t
                        :as "list")
             (file :via ,(s-prefix "todo:todoFile")
                   :as "file"))
  :resource-base (s-url "http://todo.home.arpa/todo-items/")
  :on-path "todo-items")

(define-resource todo-list ()
  :class (s-prefix "todo:TodoList")
  :properties `((:title :string ,(s-prefix "todo:title")))
  :has-one `((account :via ,(s-prefix "foaf:made")
                      :inverse t
                      :as "owner"))
  :has-many `((todo-item :via ,(s-prefix "todo:items")
                         :as "items"))
  :resource-base (s-url "http://todo.home.arpa/todo-lists/")
  :on-path "todo-lists")

(define-resource account ()
  :class (s-prefix "foaf:OnlineAccount")
  :properties `((:account-name :string ,(s-prefix "foaf:accountName")))
  :has-many `((todo-list :via ,(s-prefix "foaf:made")
                         :as "todo-lists")
              (todo-item :via ,(s-prefix "foaf:made")
                         :as "todo-items"))
  :on-path "accounts")

(define-resource file ()
  :class (s-prefix "nfo:FileDataObject")
  :properties `((:name :string ,(s-prefix "nfo:fileName"))
                (:format :string ,(s-prefix "dct:format"))
                (:size :number ,(s-prefix "nfo:fileSize"))
                (:extension :string ,(s-prefix "dbpedia:fileExtension"))
                (:created :datetime ,(s-prefix "dct:created")))
  :has-one `((file :via ,(s-prefix "nie:dataSource")
                   :inverse t
                   :as "download")
             (todo-item :via ,(s-prefix "todo:todoFile")
                        :inverse t
                        :as "todo-item"))
  :resource-base (s-url "http://todo.home.arpa/files/")
  :features `(include-uri)
  :on-path "files")

