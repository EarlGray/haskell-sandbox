import Happstack.Server
import Happstack.Server.SimpleHTTPS
import Happstack.Server.Internal.TLS

import qualified Text.Blaze.Html5 as H

tlsConf = nullTLSConf { 
    tlsPort = 8443,
    tlsCert = "web_rsa.cert",
    tlsKey = "web_rsa"
}

route = ok $ H.html $ H.h3 $ H.toHtml "TLS connection"

main :: IO ()
main = simpleHTTPS tlsConf route
