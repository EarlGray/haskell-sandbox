import Happstack.Server
import Happstack.Server.SimpleHTTPS

import qualified Text.Blaze.Html5 as H

tlsConf = nullTLSConf { 
    tlsPort = 8443,
    tlsCert = "web_rsa.cert",
    tlsKey = "web_rsa"
}

route = ok $ H.html $ H.h1 $ H.toHtml "TLS connection"

main :: IO ()
main = simpleHTTPS tlsConf route
