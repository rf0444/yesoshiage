module Handler.Root where
import Import
import Yesod.Auth
import Settings.StaticFiles

-- This is a handler function for the GET request method on the RootR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getRootR :: Handler RepHtml
getRootR = do
    mauth <- maybeAuth
    setSession "hoge" "fuga"
    sess <- getSession
    defaultLayout $ do
        addStylesheet $ StaticR bootstrap_css
        h2id <- lift newIdent
        setTitle "yesoshiage homepage"
        $(widgetFile "homepage")

getRegisterR :: Handler RepHtml
getRegisterR = do
    defaultLayout $ do
        h2id <- lift newIdent
        setTitle "新規メモ登録"
        $(widgetFile "register")

postRegisterR :: Handler RepHtml
postRegisterR = do
    memo <- runInputPost $ Memo 
              <$> ireq textField "memo"
    mid <- runDB $ do
        m <- insert $ memo
        return $ m
    let 
    defaultLayout $ do
        h2id <- lift newIdent
        setTitle "新規メモ登録"
        $(widgetFile "registerpost")

getDisplayR :: MemoId -> Handler RepHtml
getDisplayR mid = do
    memo <- runDB $ do
        m <- get404 mid
        return m
    defaultLayout $ do
        h2id <- lift newIdent
        setTitle "メモ表示"
        $(widgetFile "display")


getSessionShowR :: Handler RepHtml
getSessionShowR = do
    sess <- getSession
    value <- lookupSession "key"
    defaultLayout $ do
        setTitle "SessionShowPage"
        $(widgetFile "session-show")

getSessionFormR :: Handler RepHtml
getSessionFormR = do
    defaultLayout $ do
        setTitle "SessionFormPage"
        $(widgetFile "session-form")

postSessionRegisterR :: Handler RepHtml
postSessionRegisterR = do
    val <- runInputPost $ ireq textField "val"
    setSession "key" val
    defaultLayout $ do
        setTitle "SessionRegisterPage"
        $(widgetFile "session-register")

memoForm :: Html -> MForm YO YO (FormResult Memo, Widget)
memoForm = renderDivs $ Memo
    <$> areq textField "contents" Nothing

getFormtestR :: Handler RepHtml
getFormtestR = do
    ((_, widget), enctype) <- runFormPost memoForm
    let contentsFromRequest = (""::String)
    defaultLayout $ do
        setTitle "Memo"
        $(widgetFile "memo")

postFormtestR :: Handler RepHtml
postFormtestR = do
    ((FormSuccess memo, widget), enctype) <- runFormPost memoForm
    let contentsFromRequest = memoContents memo
    defaultLayout $ do
        setTitle "Memo"
        $(widgetFile "memo")

