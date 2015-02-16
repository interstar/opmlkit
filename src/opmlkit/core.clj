(ns opmlkit.core
  (require [clojure.xml :as xml]
           [clojure.zip :as zip] )
)


(defn parse [s]
   (xml/parse
    (java.io.ByteArrayInputStream. (.getBytes s))))

(defn tags [match opml] (filter (fn [x] (= (:tag x) match)) (:content opml) ))
(defn first-tag [match opml] (first (tags match opml)))

(defn grab-head [opml] (first-tag :head opml))

(defrecord Head [title date-modified expansion-state])

(defn get-head [opml]
  (let [h (grab-head opml)
        title (apply str (->> h (first-tag :title) :content ))
        date-mod (apply str (->> h (first-tag :dateModified) :content))
        expansion-state (apply str (->> h (first-tag :expansionState) :content))]
    (->Head title date-mod expansion-state)
    )  )

(defn grab-body [opml] (get (:content opml) 1) )

(defn xml-outline-to-internal-opml [outline]
  (if (nil? outline) nil
      (let [data {:text (->> outline :attrs :text)
                  :created (->> outline :attrs :created)}]
        (cons data (map xml-outline-to-internal-opml (:content outline)))
        ))  )

(defn get-body [opml]
  (let [body (grab-body opml)
        outline (first-tag :outline body)]
    (xml-outline-to-internal-opml outline )
    ))

(defrecord Outline [head body])

(defn make-opml [s]
  (let [xml (parse s)]
    (->Outline (get-head xml) (get-body xml))))


(defn outline-tag-to-xml
  ([outline depth]
     (if (nil? outline) ""
       (let [item (first outline)
             subs (rest outline)]
         (str
          (apply str (repeat depth "\t") )
          "<outline text='"
          (:text item)
          "' created='"
          (:created item)
          "'>\n"
          (apply str (map #(outline-tag-to-xml % (+ depth 1)) subs))
          "</outline>"))) )
  ([outline] (outline-tag-to-xml outline 0)))

(defn as-xml [outline]
  (let [head (:head outline)
        body (:body outline)
        xml-head (str "\t<head>\n\t\t<title>" (:title head)  "</title>\n\t\t<dateModified>" (:date-modified head)
                      "</dateModified>\n\t\t<expansionState>" (:expansion-state head) "</expansionState>\n\t\t</head>\n")
        xml-body (str "\t<body>" (outline-tag-to-xml body) "\n\t\t</body>")]
    (str "<?xml version='1.0'?>
<opml version='2.0'>\n" xml-head
xml-body
"\n</opml>"
))
  )

(defn pp-outline
  ([outline depth]
     (let [item (first outline)
           subs (rest outline)]
       (str
        (apply str (repeat depth "  ") )
        (:text item)
        "\n"
        (apply str (map #(pp-outline % (+ depth 1)) subs)) )) )
  ([outline] (pp-outline outline 0)))

(defn -main [& args ]
  (let [o1 (make-opml (slurp (first args)))
        o2 (make-opml (slurp (second args)))
        ]

    (println (pp-outline (:body o1)))
    )
  )
