(ns test
  (:require [clojure.java.io :as io])
  (:require [clojure.data.xml :as xml])
  )

(with-open [rdr (io/reader "C:/Users/Admin/Desktop/server.log")]
  (let [
        regExp #"(\d*-\d*-\d* \d*:\d*:\d*,\d*) (\[\w*-\d*\]) (\w*)  (\[ServiceProvider\]): (.* (request startRendering|startRendering returned|getRendering returned) .*)"
        regExpAttr #"\[(\d*), (\d*)\]"
        uuidRegExp #"returned (\d*-\d*)"
        structuredLogList (seq (map (fn [x] {
                                             :time   ((re-find regExp x) 1)
                                             :thread ((re-find regExp x) 2)
                                             :level  ((re-find regExp x) 3)
                                             :who    ((re-find regExp x) 4)
                                             :msg    ((re-find regExp x) 5)
                                             })
                                    (vec (filter (fn [x] (
                                                           and
                                                           (.contains x "ServiceProvider")
                                                           (or
                                                             (.contains x "Executing request startRendering")
                                                             (.contains x "Service startRendering returned")
                                                             (.contains x "Service getRendering returned")
                                                             )
                                                           )
                                                   )
                                                 (vec (map (fn [x] x) (line-seq rdr))))
                                         )
                                    )
                               )
        renderList (seq (
                          loop [acc []
                                collection structuredLogList
                                elem (first collection)
                                ]
                          (if (= (first collection) nil)
                            acc
                            (recur
                              (cond
                                (.contains ((first collection) :msg) "Executing request startRendering")
                                (let [
                                      arrgs (re-find regExpAttr ((first collection) :msg))
                                      filteredAcc (filter (fn [x] (and (= (x :id) (arrgs 1)) (= (x :page) (arrgs 2))
                                                                       )) acc)
                                      ]
                                  (if (= (first filteredAcc) nil)
                                    (conj acc {
                                               :id     (arrgs 1)
                                               :page   (arrgs 2)
                                               :uuid   nil
                                               :thread [((first collection) :thread)]
                                               :start  [((first collection) :time)]
                                               :get    []
                                               }
                                          )
                                    (conj (remove (fn [x] (= (first filteredAcc) x)) acc) {
                                                                                           :id     ((first filteredAcc) :id)
                                                                                           :page   ((first filteredAcc) :page)
                                                                                           :uuid   ((first filteredAcc) :uuid)
                                                                                           :thread (conj ((first filteredAcc) :thread) ((first collection) :thread))
                                                                                           :start  (conj ((first filteredAcc) :start) ((first collection) :time))
                                                                                           :get    ((first filteredAcc) :get)
                                                                                           })
                                    )
                                  )

                                (.contains ((first collection) :msg) "Service startRendering returned") (let [
                                                                                                              filteredAccByThread (filter (fn [x] (.contains (x :thread) (elem :thread))) acc)
                                                                                                              ]
                                                                                                          (if (= (first filteredAccByThread) nil)
                                                                                                            acc
                                                                                                            (conj (remove (fn [x] (= (first filteredAccByThread) x)) acc) {
                                                                                                                                                                           :id     ((first filteredAccByThread) :id)
                                                                                                                                                                           :page   ((first filteredAccByThread) :page)
                                                                                                                                                                           :uuid   ((re-find uuidRegExp (elem :msg)) 1)
                                                                                                                                                                           :thread ((first filteredAccByThread) :thread)
                                                                                                                                                                           :start  ((first filteredAccByThread) :start)
                                                                                                                                                                           :get    ((first filteredAccByThread) :get)
                                                                                                                                                                           })
                                                                                                            )
                                                                                                          )
                                (.contains ((first collection) :msg) "Service getRendering returned") (let [
                                                                                                            filteredAccByThread (filter (fn [x] (.contains (x :thread) (elem :thread))) acc)
                                                                                                            ]
                                                                                                        (if (= (first filteredAccByThread) nil)
                                                                                                          acc
                                                                                                          (conj (remove (fn [x] (= (first filteredAccByThread) x)) acc) {
                                                                                                                                                                         :id     ((first filteredAccByThread) :id)
                                                                                                                                                                         :page   ((first filteredAccByThread) :page)
                                                                                                                                                                         :uuid   ((first filteredAccByThread) :uuid)
                                                                                                                                                                         :thread (remove (fn [x] (= (elem :thread) x)) ((first filteredAccByThread) :thread))
                                                                                                                                                                         :start  ((first filteredAccByThread) :start)
                                                                                                                                                                         :get    (conj ((first filteredAccByThread) :get) (elem :time))
                                                                                                                                                                         })
                                                                                                          )
                                                                                                        )

                                :else acc)
                              (rest collection)
                              (first (rest collection))
                              )
                            )
                          ))
        ]
    (doseq [line renderList] (println line))

    )
  )

;(let [
;      str "010-10-06 09:03:06,547 [WorkerThread-15] INFO  [ServiceProvider]: Executing request getRendering with arguments [1286373785873-3536] on service object { ReflectionServiceObject -> com.dn.gaverzicht.dms.services.DocumentService@4a3a4a3a }"
;      regRxx #"(\d*-\d*-\d* \d*:\d*:\d*,\d*) (\[\w*-\d*\]) (\w*)  (\[ServiceProvider\]): (.* (request getRendering with|request startRendering|startRendering returned|getRendering returned) .*)"
;      ]
;  (println ((re-find regRxx str) 5))
;(let [
;      coller []
;      answer (if (= (first coller) nil) "yes" "no")
;      ]
;  (print answer)
;(let [
;      str "2010-10-07 10:56:59,936 [WorkerThread-8] INFO  [ServiceProvider]: Service getRendering returned java.io.FileInputStream@4d984d98"
;      regExp #"(\d*-\d*-\d* \d*:\d*:\d*,\d*) (\[\w*-\d*\]) (\w*)  (\[ServiceProvider\]): (.* (request startRendering|startRendering returned|getRendering returned) .*)"
;      regExpMatcher (re-matcher #"(\d*-\d*-\d* \d*:\d*:\d*,\d*) (\[\w*-\d*\]) (\w*)  (\[ServiceProvider\]): (.* (request startRendering|startRendering returned|getRendering returned) .*)" str)
;      ]
;    (println ((re-find regExp str) 0))
;  )
;(with-open [rdr (io/reader "C:/Users/Admin/Desktop/server.log")]
;  (def strList [1 2 3])
;  (doseq  [line ()]
;      (print line)
;    )
;  )

;(defn my-re-seq [re string]
;  (let [matcher (re-matcher re string)]
;
;    (loop [match (re-find matcher)
;           result []]
;      (if-not match
;        result
;        (recur (re-find matcher)
;               (conj result match))))))
;
;
;  (def listTwo (my-re-seq #"(\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2},\d{3}) (\[\w*-\d*\]) (\w*)  (\[ServiceProvider\]): (.* (request startRendering|startRendering returned|getRendering returned) .*)" (slurp "C:/Users/Admin/Desktop/server.log")))
; (doseq [item listTwo]
;    (println item)
;   )
;(print filter (fn [el] (.contains el "startRendering")) (my-re-seq #"(\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2},\d{3}) (\[\w*-\d*\]) (\w*)  (\[\w*\]): (.*)" (slurp "C:/Users/Admin/Desktop/server.log")))
