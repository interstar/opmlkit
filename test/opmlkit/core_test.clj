(ns opmlkit.core-test
  (:require [clojure.test :refer :all]
            [opmlkit.core :refer :all]))

(deftest test-tag
  (testing "Testing the tag and first tag"
    (let [simple-xml "<tagA><tagB>Hello Teenage America</tagB></tagA>"
          simple-p (parse simple-xml)          ]
      (is (= simple-p {:tag :tagA, :attrs nil, :content [{:tag :tagB, :attrs nil, :content ["Hello Teenage America"]}]} ))
      (is (= (tags :tagB simple-p) '( {:tag :tagB, :attrs nil, :content ["Hello Teenage America"]}) ))
      (is (= (first-tag :tagB simple-p) {:tag :tagB, :attrs nil, :content ["Hello Teenage America"]} ))
      )))

(deftest test-xml-to-native
    (testing "Testing xml to native"
      (let [outline {:tag :outline, :attrs {:text "<a href='#ObjectOriented'>ObjectOriented</a>",
                                            :created "Wed, 02 Oct 2013 21:04:39 GMT"}, :content nil}
            out2 {:tag :outline, :attrs {:text "<a href='#ObjectOriented'>ObjectOriented</a>",
                                         :created "Wed, 02 Oct 2013 21:04:39 GMT"},
                  :content [{:tag :outline, :attrs {:text "SECOND LEVEL", :created "BEEP"}, :content nil}]}
            out3 {:tag :outline, :attrs {:text "<a href='#ObjectOriented'>ObjectOriented</a>",
                                         :created "Wed, 02 Oct 2013 21:04:39 GMT"},
                  :content [{:tag :outline, :attrs {:text "SECOND LEVEL", :created "BEEP"},
                             :content [{:tag :outline, :attrs {:text "THIRD LEVEL", :created "BOOP"}, :content nil}]}]}
            out4 {:tag :outline, :attrs {:text "<a href='#ObjectOriented'>ObjectOriented</a>",
                                         :created "Wed, 02 Oct 2013 21:04:39 GMT"},
                  :content [{:tag :outline, :attrs {:text "SECOND LEVEL", :created "BEEP"}, :content nil}
       {:tag :outline, :attrs {:text "ANOTHER 2nd", :created "BRAH"}, :content nil}  ]}   ]
        (is (= (xml-outline-to-internal-opml outline)
               '({:text "<a href='#ObjectOriented'>ObjectOriented</a>"
                  :created "Wed, 02 Oct 2013 21:04:39 GMT"})))

        (is (= (xml-outline-to-internal-opml out2)
               '({:text "<a href='#ObjectOriented'>ObjectOriented</a>"
                  :created "Wed, 02 Oct 2013 21:04:39 GMT"} ({:text "SECOND LEVEL" :created "BEEP"}) )))

        (is (= (xml-outline-to-internal-opml out3)
               '({:text "<a href='#ObjectOriented'>ObjectOriented</a>"
                  :created "Wed, 02 Oct 2013 21:04:39 GMT"}
                 ({:text "SECOND LEVEL" :created "BEEP"}
                  ({:text "THIRD LEVEL" :created "BOOP"})) )))

        (is (= (xml-outline-to-internal-opml out4)
               '({:text "<a href='#ObjectOriented'>ObjectOriented</a>"
                  :created "Wed, 02 Oct 2013 21:04:39 GMT"}
                 ({:text "SECOND LEVEL" :created "BEEP"})
                 ({:text "ANOTHER 2nd" :created "BRAH"}) )))
        ))
    )


(let [xml  "<?xml version='1.0'?>
<opml version='2.0'>
	<head>
		<title>A Page</title>
		<dateModified>Sat, 07 Feb 2015 16:40:16 GMT</dateModified>
		<expansionState>1,2,3,5,8</expansionState>
		</head>
	<body>
		<outline text='Art Projects  ' created='Wed, 06 Nov 2013 21:31:33 GMT'>
			<outline text='visual' created='Thu, 16 Oct 2014 05:44:52 GMT'>
				<outline text='&lt;a href=&quot;#ClojurePatterning&quot;&gt;ClojurePatterning&lt;/a&gt;  ' created='Mon, 21 Apr 2014 01:41:30 GMT'/>
				</outline>
			</outline>
		</body>
	</opml>
"
      opml (parse xml ) ]

  (deftest test-head
    (testing "Testing the head"
      (is (= (grab-head opml) {:tag :head, :attrs nil, :content [{:tag :title, :attrs nil, :content ["A Page"]} {:tag :dateModified, :attrs nil, :content ["Sat, 07 Feb 2015 16:40:16 GMT"]} {:tag :expansionState, :attrs nil, :content ["1,2,3,5,8"]}]} ))
      (is (= (get-head opml) (->Head "A Page" "Sat, 07 Feb 2015 16:40:16 GMT" "1,2,3,5,8")))
      ))


  (deftest test-body
    (testing "Testing the body"
      (is (= (grab-body opml) {:tag :body, :attrs nil, :content [{:tag :outline, :attrs {:text "Art Projects  ", :created "Wed, 06 Nov 2013 21:31:33 GMT"}, :content [{:tag :outline, :attrs {:text "visual", :created "Thu, 16 Oct 2014 05:44:52 GMT"}, :content [{:tag :outline, :attrs {:text "<a href=\"#ClojurePatterning\">ClojurePatterning</a>  ", :created "Mon, 21 Apr 2014 01:41:30 GMT"}, :content nil}]}]}]} ))
      (is (= (get-body opml) '({:text "Art Projects  " :created "Wed, 06 Nov 2013 21:31:33 GMT" } ({:text "visual" :created "Thu, 16 Oct 2014 05:44:52 GMT"} ({:text "<a href=\"#ClojurePatterning\">ClojurePatterning</a>  "  :created "Mon, 21 Apr 2014 01:41:30 GMT" }) ) )))
      ))

  (deftest test-into-outof
    (testing "Testing convert XML to and from internal format"
      (let [test-it (fn [s] (println s) (println (as-xml (make-opml s)))
                      (is (= (parse s) (parse (as-xml (make-opml s))))))]
        (test-it "<?xml version='1.0'?>
<opml version='2.0'>
\t<head>
\t\t<title></title>
\t\t<dateModified></dateModified>
\t\t<expansionState></expansionState>
\t\t</head>
\t<body>
\t\t</body>
</opml>")
        (test-it xml )
        )      ))
  )
