{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-name-shadowing #-}

-- | Load the configuration file.

module Hpaste.Config
       (getConfig)
       where

import Hpaste.Types.Config


import Data.ConfigFile
import Database.PostgreSQL.Simple (ConnectInfo(..))
import qualified Data.Text as T
import Network.Mail.Mime

getConfig :: FilePath -> IO Config
getConfig conf =
  do contents <- readFile conf
     let config =
           do c <- readstring emptyCP contents
              [user,pass,host,port] <-
                mapM (get c "ANNOUNCE")
                     ["user","pass","host","port"]
              [pghost,pgport,pguser,pgpass,pgdb] <-
                mapM (get c "POSTGRESQL")
                     ["host","port","user","pass","db"]
              [domain,cache] <-
                mapM (get c "WEB")
                     ["domain","cache"]
              [commits,url] <-
                mapM (get c "DEV")
                     ["commits","repo_url"]
              [ircDir] <-
                mapM (get c "IRC")
                     ["log_dir"]
              [admin,siteaddy] <-
                mapM (get c "ADDRESSES")
                     ["admin","site_addy"]
              [key] <-
                mapM (get c "ADMIN")
                     ["key"]
              return Config {configAnnounce =
                               AnnounceConfig user
                                              pass
                                              host
                                              (read port)
                            ,configPostgres =
                               ConnectInfo pghost
                                           (read pgport)
                                           pguser
                                           pgpass
                                           pgdb
                            ,configDomain = domain
                            ,configCommits = commits
                            ,configRepoURL = url
                            ,configIrcDir = ircDir
                            ,configAdmin = Address Nothing (T.pack admin)
                            ,configSiteAddy = Address Nothing (T.pack siteaddy)
                            ,configCacheDir = cache
                            ,configKey = key}
     case config of
       Left cperr -> error $ show cperr
       Right config -> return config
