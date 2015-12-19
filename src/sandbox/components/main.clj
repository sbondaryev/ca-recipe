(ns sandbox.component.main)

(defn [twitter-chan (async/chan 100)
       twitter-response-chan (async/chan 10)
       facebook-chan (async/chan 100)
       facebook-response-chan (async/chan 10)
       alert-chan (async/chan 100)
       response-chan (async/chan 100)
       feed-chan (async/merge [twitter-chan facebook-chan])
       response-pub (async/pub response-chan :feed)]
  (async/sub response-pub :twitter twitter-response-chan)
  (async/sub response-pub :facebook facebook-response-chan)

  (component/system-map
   :twitter (feed/new-feed twitter twitter-chan twitter-response-chan)
   :facebook (feed/new-feed facebook facebook-chan facebook-response-chan)
   :knowledge-engine
   (knowkedge/new-knowledge-engine knowledge feed-chan alert-chan)
   :approvals (component/using
               (approvals/new-approvals approvals alert-chan response-chan)
               [:knowledge-engine])))

