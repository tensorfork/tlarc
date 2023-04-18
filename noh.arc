(load (libpath "news.arc") :once)
(load (libpath "srp.arc") :once)
(load (libpath "php.arc") :once)

(or= noh-chat-url* "127.0.0.1" ; local chat server testing
     noh-chatrooms* (list "NoH" "test")
     )

(addtem profile
  uid       nil
  nickname  nil
  )

;(def uid->user* (uid)
;  ; TODO: turn this into a table?
;  (when uid
;    (each (u p) profs*
;      (when (is uid p!uid)
;        (break u)))))

(or= uid->user* (table))

(diskvar maxuid* (+ newsdir* "maxuid") 0)

(def set-maxuid ((o i maxuid*))
  (= maxuid* i)
  (todisk maxuid*)
  i)

(def new-user-id ()
  (set-maxuid (+ 1 maxuid*)))

(defhook load-user name: noh (u)
  (unless (uvar u uid)
    (= (uvar u uid) (new-user-id))
    (save-prof u))
  (awhen (uvar u uid)
    (= maxuid* (max maxuid* it)
       (uid->user* it) u)))

(defhook loaded name: noh ()
  (todisk maxuid*))

(defhook user-fields name: noh (subject)
  (withs (e (editor)
          a (admin)
          w (is-user subject)
          k (or a (and w (> (karma) topcolor-threshold*)))
          u (or a w)
          m (or a (and (member) w))
          p (profile subject)
          s subject)
    (w/accum
      `(int     uid        ,(p 'uid)                                ,a  nil) ; don't make this editable (can't easily update uid->user*)
      `(string  nickname   ,(p 'nickname)                           ,m  ,m)
      )))

(def noh-requester (kind req)
  (ero req)
  (let op arg!f
    (aif (nohop-fn kind op)
         (aand (it (outstring) req)
               (pr:php-encode it))
         (pr "Unsupported Endpoint \"/@{kind}_requester.php?f=@op\""))))

(defop client_requester.php req (noh-requester 'client req))

(defop server_requester.php req (noh-requester 'server req))

(mac noh-opexpand (definer name parms . body)
  `(,definer ,name ,(uniq 'req)
     (withs (user (get-user) ip (get-ip))
       (withs ,(and parms (mappend [list _ `(arg ,(string _))]
                                   parms))
         (newslog ',name ,@(mappend [list (+ (string _) ":") `(tostring:write-json ,_)]
                                    parms))
         ,@body))))

(or= nohop-names* (table))

(mac nohop (kind name args . body)
  `(do (pushnew ',name (nohop-names* ',kind))
       (noh-opexpand defop ,(+ 'noh-- kind '-- name) ,args
         ,@body)))

(def nohop-fn (kind name)
  (srvops* (+ 'noh-- kind '-- name)))

;(mac nohopr args
;  `(do (pushnew ',(car args) nohop-names*)
;       (opexpand defopr ,@args)))

(def user-nickname ((o user (get-user)))
  (aif (uvar user nickname)
       (if (empty it) user it)
       user))

(def user-account-id ((o user (get-user)))
  (uvar user uid))

;enum EAccountType
;{
;	ACCOUNT_DISABLED,
;	ACCOUNT_DEMO,
;	ACCOUNT_SERVER,
;	ACCOUNT_REGULAR,
;	ACCOUNT_PREMIUM,
;	ACCOUNT_STAFF,
;	ACCOUNT_GM,

;	NUM_ACCOUNT_TYPES
;};

(def noh-client-login ((o user (get-user)) (o cookie get-cookie!user))
  (if cookie
      (obj account_type (if (admin user)
                            5 ; staff
                            3 ; regular
                            )
           account_id (user-account-id user)
           nickname (user-nickname user)
           email (uvar user email)
           ip (get-ip)
           cookie cookie
           chat_url noh-chat-url*
           chatrooms noh-chatrooms*
           )
      (obj error '("invalid credentials"))))

(nohop client pre_auth (login A SysInfo)
  (iflet info (srpasswords* login)
         (obj salt info!salt
              salt2 info!salt2
              B (k2-auth login A
                         (fn (user ok)
                           (noh-client-login user (if ok (cook-user! user))))))
         (err "No such user.")))

(nohop client srpAuth (login proof SysInfo)
  (aif (k2-auth-fn login)
       (it proof)
       (obj error '("invalid preauth"))))

(nohop client server_list ()
  (obj acc_key "todo"
       svr_stats (obj online 69 in_game 420)
       server_list ()))

(nohop client chatclient_connect (account_id cookie chat_mode_type token)
  (set-cookie 'user cookie)
  (aif (get-user)
       (do ;(dbg)
           ; HoN Chat Server/Chat Server/c_pendingconnection.cpp:169
           (obj nickname (user-nickname it)
                account_id (user-account-id it)
                chat_mode_type (int chat_mode_type) ; 0 = available, 1 = invisible
                ; TODO: stats, buddy_list, check_perms, rank, clan_id
                )
           )
       (obj error '("invalid cookie"))))


(nohop server accept_key (session acc_key)
  (obj server_id 420))

(nohop server new_session (login ip port location name desc)
  (aif (good-login login arg!pass)
       (do ;(dbg)
           (obj server_id 420
                session get-cookie!user))
       (obj error (list "Invalid login"))))

(nohop server set_online (session num_conn cgt private c_state prev_c_state)
  (set-cookie 'user session)
  (aif (get-user)
       (do ;(dbg)
           (obj authenticate "OK"))
       (obj authenticate "Failed to authenticate B1")))
   

run-news

