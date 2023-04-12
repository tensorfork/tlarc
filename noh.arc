(load (libpath "news.arc") :once)

(def php-encode-string (x)
  (let s (string x)
    (+ "s:" (len s) ":\"" s "\";")))

(def php-encode-kv ((k v))
  (+ (php-encode k)
     (php-encode v)))

(def arrlist (l)
  (let i 0
    (each x l
      (out (list i x))
      (++ i))))

(def php-encode-pairs (kvs)
  (+ "a:@(len kvs):{" (mapcat php-encode-kv kvs) "}"))

(def php-encode (x)
  (case (type x)
    num          "d:@x;"
    int          "i:@x;"
    bool         (+ "b:" (if x "true" "false") ";")
    (sym string) (php-encode-string x)
    cons         (php-encode-pairs (arrlist x))
    table        (php-encode-pairs (tablist x))
    (err "Bad type @x")))

(defop client_requester.php req
  (ero req)
  (let op arg!f
    (aif (nohop-fn 'client op)
         (aand (it (outstring) req)
               (pr:php-encode it))
         (pr "Unsupported Endpoint \"/client_requester?f=@op\""))))

(mac noh-opexpand (definer name parms . body)
  `(,definer ,name ,(uniq 'req)
     (withs (user (get-user) ip (get-ip))
       (withs ,(and parms (mappend [list _ `(arg ,(string _))]
                                   parms))
         (newslog ',name ,@parms)
         ,@body))))

(or= nohop-names* (table))

(mac nohop (kind name args . body)
  `(do (pushnew ',name (nohop-names* ',kind))
       (noh-opexpand defop ,(+ 'noh- kind '- name) ,args
         ,@body)))

(def nohop-fn (kind name)
  (srvops* (+ 'noh- kind '- name)))

;(mac nohopr args
;  `(do (pushnew ',(car args) nohop-names*)
;       (opexpand defopr ,@args)))

(or= srp-auth* (table))

(def k2-auth-from-A (A)
  (obj A (hexint A)))

(def k2-auth (user (o auth))
  (or= auth (srp-start-auth))
  (def a auth!a)
  (def A auth!A)
  (def b (hexint:randhex 1024))
  (def res (k2-server-start-auth user b))
  (def B res!B)
  (fnidf (fn (proof)
           (withs (a auth!a
                   A auth!A
                   x (hexint:srpvar user x)
                   s (k2-salt user)
                   S (k2-server-S A B b x)
                   M (k2-M user s A B S))
             (dbg M)))
         (list 'k2-auth user))
  B)

(nohop client pre_auth (login A SysInfo)
  (iflet info (srpasswords* login)
         (obj salt info!salt
              salt2 info!salt2
              B (inthex:k2-auth login (k2-auth-from-A A)))
         (err "No such user.")))

(nohop client srpAuth (login proof SysInfo)
  (aand (fnid-key (list 'k2-auth login))
        (fns*:sym:fnkeys* it)
        (it proof)))

(def byteshex (x)
  (mapcat hexreps x))

;(def hexbytes (x)
;  (if (isa!cons x) x
;    (forlen i x
;      (aand (string (x i)
;                    (x (++ i)))
;            (out:int it 16)))))

(def hexbytes (x)
  (pad (intbytes:hexint x) (idiv (len x) 2)))

(def hexint (x)
  (int x 16))

(def inthex (i)
  (aand (coerce i 'string 16)
        (if (even:len it) it
            (+ "0" it)
            it)))


(def rshift (i by)
  (#'arithmetic-shift i (- by)))

(def lshift (i by)
  (#'arithmetic-shift i by))

(def int-to-bytes (n)
  (if (isa!cons n) n
    (withs (l () x 0 off 0)
      (while (isnt x n)
        (let b (bitwise-and (rshift n off) 0xFF)
          (push b l)
          (= x (bitwise-ior x (lshift b off)))
          (++ off 8)))
      l)))

(def intbytes (i (o n) (o endian 'big))
  (if n
      (hexbytes:pad (inthex i) (* 2 n) "0" right: (is endian 'little))
      (int-to-bytes i)))

(def bytesint (bs)
  (hexint:byteshex bs))

(def randhex (n)
  (byteshex:rand-bytes n))

#'(require file/md5)

(or= SHA256 #'sha256-bytes
     MD5 (let MD5 #'md5
           [MD5 _ false]))

(def sha256 (bytes :hex) (hashing SHA256 bytes :hex))
(def md5    (bytes :hex) (hashing MD5    bytes :hex))

(def sha256hex (x) (sha256 x :hex))
(def md5hex    (x) (md5    x :hex))
  
(def hashing (hasher bytes :hex)
  (aand (coerce bytes 'bytes)
        (#'list->bytes it)
        (hasher it)
        (#'bytes->list it)
        (if hex (byteshex it) it)))

;(def srp-hash (hasher . args)
;  (bytesint:hasher:mapcat [if (isa!string _) (coerce _ 'bytes) (intbytes _)] args))

(def srp-hash (hasher . args)
  (bytesint (hasher (hexbytes (concat (map byteshex (map [if (isa!string _) (coerce _ 'bytes) (intbytes _)] args)))))))

(def k2-H args
  (apply srp-hash sha256 args))

(= k2-N* (hexint "da950c6c97918cae89e4f5ecb32461032a217d740064bc12fc0723cd204bd02a7ae29b53f3310c13ba998b7910f8b6a14112cbc67bdd2427edf494cb8bca68510c0aaee5346bd320845981546873069b337c073b9a9369d500873d647d261cced571826e54c6089e7d5085dc2af01fd861ae44c8e64bca3ea4dce942c5f5b89e5496c2741a9e7e9f509c261d104d11dd4494577038b33016e28d118ae4fd2e85d9c3557a2346faeced3edbe0f4d694411686ba6e65fee43a772dc84d394adae5a14af33817351d29de074740aa263187ab18e3a25665eacaa8267c16cde064b1d5af0588893c89c1556d6aef644a3ba6ba3f7dec2f3d6fdc30ae43fbd6d144bb")
   k2-g* 2)

(def k2-Ng () (list k2-N* k2-g*))
(def k2-N () k2-N*)
(def k2-g () k2-g*)

#'(require math)

(def pow (a b (o n))
  (if n
      (modular-expt a b n)
      (expt a b)))

; M = H(H(N) xor H(g), H(I), s, A, B, K)

(mac srp-k2 (f . args)
  `(,f sha256 (k2-N) (k2-g) ,@args))

(mac srp-k2def (name args)
  `(def ,(+ 'k2- name) ,args
     (srp-k2 ,(+ 'srp- name) ,@args)))

(defmemo srp-H (hasher)
  (def H args
    (apply srp-hash hasher args))
  H)

(mac defsrp (name args . body)
  `(do (def ,(+ 'srp- name) (hasher N g ,@args)
         (def H (srp-H hasher))
         ,@body)
       (srp-k2def ,name ,args)))

(mac srp% (f . args)
  `(,(+ 'srp- f) hasher N g ,@args))

(defsrp HNxorg ()
  (map [apply bitwise-xor _]
       (zip (hasher:intbytes N)
            (hasher:intbytes g 256))))

(assert-is (byteshex:k2-HNxorg)
           "ad24f7f2c5a23e7f193218290dc94861225c4af9d12145f174a4fa9113ba2999")

(defsrp k ()
  (H N (intbytes g 256)))

(assert-is (k2-k) 65007407710358415961854757974899363834884682302257814653619811249861271553835)

(defsrp v (x)
  (pow g (hexint x) N))

(assert-is (k2-v 72361523276681151397632702657762213634711426708154335164759540844463839812825)
           9287622305195707196230457813178595003302211288559972844761754886121358438287382921688654375533185583222525813146399674724501956774563766347413635175239529269156851634390910227089798062696982389393688998600499881713011369627280259168192445683538027995805715473978307434256487104027713984718606247696575360445941203910345143747982816828709135830145657943433360234461892847830006634313282727286030484072061915137310145366234101501306960334051578610990189167619675209282125639290645533119562495174895740223733697583681472503726236718624361560405019244403192426541201804822449378115414394418381216116442353472969609673965)

(defsrp u (A B)
  (H (hexint A)
     (hexint B)))

(assert-is (k2-u 19408989542663554039483436235577659287646139567166578397728551191813750812501951290617698897122681009200214174535233211743181714298774868851110874723156897152670620661356195262861731871468928670092490424705769792504450330952673021702217542237009058315387096101465940340376906104315320750231016405826336039742482856311392523455374224485499991604894540767695277839882081541647552684665474823006946198813801702315081733479082596847347474179856922089695079669141445260367498134033985233589138061151815376381286457177873006191992752837846557830827885013203753111165644142351042603277052577070212068027913418809604860010887
                 3361213721291489647736555504260365640689983911623135488796423935035254142942987649761155963210885986387616695829235005163969417988401427246526851174752151298677256026585736730373933749026860503130135193554480667172786618860343115641409446983064562426395560097658246421536812518273884039303147943757599013544725496197000664314813972783121514836388607770708599495726764221452795861287854551472818894369557696474256252021291326676664235676837611816339428102210417895313683176487268800270676555794005126497555260815935616801362281063484517224918583048913290487742612179928257513611306366565075776281627387068357875846946)
           69755991518957955667396784184622249707808244100980088293742500184066772435395)

(defsrp K (S)
  (H (hexint S)))

(assert-is (k2-K 7921157424712253028337981611980857888125629161825289797643042003839044782244142970648976602855512878947973768911446290069320282176360053573708433324564628536003363037415287596221780121083970064531477993332725117444540298503870691357144506180326797716898385187303607125825045316278794945960524473452236497207486243346264424632133153924916334132999700967521311240683601561789092509781258146919801511483407272512160881041809993388421005568407337215613873870351904677558573802631693910727105857702550377032561691563555212524352229475473244751060760273368402227594679884744366691927826588640161694621504743551908280292886)
           94478099561475054875201543270246831600417731325257874224073145334936878048285)

; K = H(S)

; S = (Av^u) ^ b 
(defsrp server-S (A B b x)
  (def u (srp% u A B))
  (def v (srp% v x))
  (pow (* A (pow v u N)) b N))

; S = (B - kg^x) ^ (a + ux)
(defsrp client-S (A B a x)
  (def u (srp% u A B))
  (def v (srp% v x))
  (def k (srp% k))
  (pow (- B (* k v))
       (+ a (* u x))
       N))

(defsrp server-x (user)
  (hexint:srpvar user x))

(def calculate-M-vals (hasher N g I s A B K)
  (list (byteshex (srp-HNxorg hasher N g))
        (byteshex (hasher I))
        (inthex:hexint s)
        (inthex:hexint A)
        (inthex:hexint B)
        (inthex:hexint K)))


; broken...
;(def calculate-M (hasher N g I s A B K)
;  (srp-hash hasher
;            (srp-HNxorg hasher N g)
;            (hasher I)
;            (hexint s)
;            (hexint A)
;            (hexint B)
;            K))

(def calculate-M (hasher N g I s A B K)
  (let xs (calculate-M-vals hasher N g I s A B K)
    (hasher:hexbytes:concat xs)))

; broken...
;(defsrp M (user salt A B S)
;  (withs (xorg (srp% HNxorg)
;          s (hexint salt)
;          A (hexint A)
;          B (hexint B)
;          K (srp% K S))
;    (H xorg (H user) s A B K)))

(defsrp M (user salt A B S)
  (calculate-M hasher N g user salt A B (srp% K S)))

(def k2-salt (user)
  (srpvar user salt))

(def k2-salt2 (user)
  (srpvar user salt2))

(def k2-x (user)
  (srpvar user x))

(def k2-pwhash (pw (o salt2))
  (or= salt2 (k2-salt2 user))
  (aand (+ (md5hex pw) (inthex salt2) "[!~esTo0}")
        (+ (md5hex it) "taquzaph_?98phab&junaj=z=kuChusu")
        (sha256hex it)))

(def k2-private-key-from-pwhash (user pwhash (o salt))
  (or= salt (k2-salt user))
  (k2-H (hexint salt) (k2-H (+ user ":" pwhash))))

(def k2-private-key-from-pw (user pw (o salt) (o salt2))
  (def pwhash (k2-pwhash pw salt2))
  (k2-private-key-from-pwhash user pwhash salt))

(def k2-set-pw (user pw (o salt) (o salt2))
  (with info (obj salt (or= salt (randhex 256))
                  salt2 (or= salt2 (randhex 11))
                  x (inthex:k2-private-key-from-pw user pw salt salt2))
    (= (srpasswords* user) info)
    (save-table srpasswords* srpwfile*)))

(= srpwfile*   (libpath "arc/srpw"))

(defhook load-userinfo () name: noh
  (= srpasswords* (safe-load-table srpwfile*))
  t)

(defhook set-pw (pw (o user (get-user))) name: noh
  (k2-set-pw pw user))

(= B (hexint "c5c410f2badc6c1ceae6c74860d697eae30c9c035d0084010c062c13017778209fde206d43beed82e0a2b9007f85bc604f64c2df1d9cb7641d81deb383c5ddc3660c3dce0d0eab46e90217b42922a0505201d7902abb719711a6bb913bd048ee2a02c1588920fd4d2c00166fb88e3354c4206c1153ce44e9ab57c09b50bf23233a768346e7c199565a910a84bc08b0522c4a318602752539b9bf3e88de6ddf2479726b643d6a44ef1a1bb84aed47670089420a45a23c170681d4a646929a4d6884b6f656ed7eecd6385df987d44468ce3588b74c2e95c105f6c6576666712ca1434d8c9009181cc709f7317a4b106be2e3db9fc0796637aaed77088de817f704")
   A (hexint "2e1329befc88bf895b5e64d0c589673b51bddd85e09d9f865cdf546885b7428bb243b3017392d7800b48b118a7020eea4286e69ecc0691753e3a4fa987ce89f34e7a3dc85f205d91ad4457789a13f9ad444a99ec23de1132407c64b17ad7ff2ec69b44de686ac9eb1128d11cc9f7ab4bcde24a6ba9fd53e8faeb5c25f47e568279556b604450a940cc57ed2dd8e3141158127b03c85000f28a3f57f8b2cad6766c66f1f062a247fe92d3d322a81ea2c06bcc16cf3bc238a8d757be1ede5b6146375ef25248fe2faa8376dae353b08484ef50388857b9cea8aa81d4565c5cbaac3d469cf0498589f1c869f00aa3049ce3bfba25319addd2ec1d9c1b5997111b14")
   salt "1319abc486d3786230984fd2dcfef9771abe4d8616e5c935b27f095018fde7852fd4a5673ba4374172e20b7b28573004e456cef643440edb664ae00cc3d6c0e7be5863d22f498fdc2fe5d07285d43bdcf7538e15f2915cb4150e658dd8d41dd38cf833e186fb50603b3aa31969a1c8dd0646f9b9dbf2688690282bb8829cfab2993b60cdee99f474ed7ef5ef89d34eefb6103849b1697f0fa47dc93ad495ead178defb2ce64a153cc11843f47f96552c4446aec3c08c66886592fa4217d3b15ad5e912d4ab107c3791a7f068ae4f1d7cd219a0da2a784b1663d0e883d159cc151c3b3f17612eadcda8adbcf1b2e6e85ce01e3c2d2ae4629ba91fba0ca173d109"
   salt2 "b10ffa53b040b309379abc"
   mod modulo)

(def srp-new-auth (A (o a))
  (obj A A a a))

(defsrp start-auth (a)
  (or= a (hexint:randhex 32))
  (def A (pow g a N))
  (srp-new-auth A a))

;(def k2-login (user (o pw) :a :b :s :s2)
;  (or= s (k2-salt user)
;       s2 (k2-salt2 user))
;  (withs (req (srp-start-auth :a)
;          a req!a
;          A req!A
;          x (if pw (k2-gen-x user pw s s2)))
;    (obj req req
;         res (k2-check-pw user A :b :s :x))))

;(def process-challenge (A p s B (o :user (get-user)) (o :pw))
;  )

;(def k2-client-login (user pw :auth)
;  (or= auth (srp-start-auth))
;  (with usr (obj user user
;                 a auth!a
;                 A auth!A)

(mac srpvar (u k)
  `((srpasswords* ,u) ',k))

;(def srp-k (hasher N g)
;  (srp-multiplier hasher N g))

;(def srp-B (hasher N g x)
;  (withs (u (srp-u hasher A B)
;          v (srp-v N g x))
;          k (srp-k hasher N g)
;          B (mod (+ (* k v)
;                    (pow g b N))
;                 N)))

;(def srp-check-pw (hasher N g A salt key (o nonce (randhex 1024)))
;  (withs (A (hexint A)
;          s (hexint salt)  ; salt
;          x (hexint key)   ; private key
;          b (hexint nonce) ; public ephemeral value
;          v (pow g x N) ; password verifier
;          k (srp-multiplier hasher N g)
;          B (

(defsrp server-start-auth (user b)
  (or= b (hexint:randhex 1024))
  (def x (hexint:srpvar user x))
  (def k (srp% k))
  (def v (srp% v x))
  (def B (mod (+ (* k v)
                 (pow g b N))
              N))
  (obj user user B B b b))

(def k2-check-pw (:auth :b :s :x (o :user (get-user)))
  (or= auth (srp-start-auth)
       b (randhex 1024)
       s (k2-salt user)
       x (k2-x user)) ; private key
  (withs ((N g) (k2-Ng)
          s (hexint s)      ; salt
          b (hexint b)      ; public ephemeral value
          x (hexint x)      ; private key
          v (pow g x N)     ; password verifier
          k (srp-multiplier sha256 N g)
          B (mod (+ (* k v)
                    (pow g b N))
                 N)
          u (H sha256 A B)  ; Random scrambling parameter
          )
    ; SRP-6a safety checks
    (unless (or (is (mod B N) 0)
                (is u 0))
      (obj user user
           a auth!a
           A auth!A
           b b
           B B
           s s
           x x))))

(def k2-server-preauth (user A)
  (k2-check-pw auth: (srp-new-auth A) user: user))

(= user "shawwn"
   pw "honPassword1!"
   x (k2-x user)
   ;A "2e1329befc88bf895b5e64d0c589673b51bddd85e09d9f865cdf546885b7428bb243b3017392d7800b48b118a7020eea4286e69ecc0691753e3a4fa987ce89f34e7a3dc85f205d91ad4457789a13f9ad444a99ec23de1132407c64b17ad7ff2ec69b44de686ac9eb1128d11cc9f7ab4bcde24a6ba9fd53e8faeb5c25f47e568279556b604450a940cc57ed2dd8e3141158127b03c85000f28a3f57f8b2cad6766c66f1f062a247fe92d3d322a81ea2c06bcc16cf3bc238a8d757be1ede5b6146375ef25248fe2faa8376dae353b08484ef50388857b9cea8aa81d4565c5cbaac3d469cf0498589f1c869f00aa3049ce3bfba25319addd2ec1d9c1b5997111b14"
   ;B (hexint "27bc5e64d8bede87205b7c435a3f8249341025d0bbf98cf307c62de82571377f20d42d2175b3573edf5e6aefd469f348411cc80795e70c1a721834a2de84d597f174441887270b4c1f99af11b51298e712212b828f2d9aa0f777afa7953f1f6ebd52fe87ce6094968e922940c1ec7b15d657cd5af54da9c2bce4aa6cf4d9deef91633216b2cd7d2438c1d9ebca757ef9a1c389e89022c1c7e86509bac547b5218f562a58910c14f7c85a99a329595c06ec18e99a49fde61f65ccf0db1274744bca70e9f396e32ff1e7bbce0cbaf7b31cc511b427fb638f34a44b4b4b3b077ff7c264816ca2fd4d8a48d3afabbcd65bae253114a305ad8b08dbf680befe070d48")
   auth (k2-start-auth nil)
   A auth!A
   a auth!a
   sauth (k2-server-start-auth user nil)
   B sauth!B
   b sauth!b
   )
   

run-news

