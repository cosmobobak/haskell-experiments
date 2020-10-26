module Tutorial4 where

import Data.Char
import Data.List (nub)
import Network.HTTP (getRequest, getResponseBody, simpleHTTP)
import Test.QuickCheck

-- <type decls>

type Link = String

type Name = String

type Email = String

type HTML = String

type URL = String

-- </type decls>
-- <sample data>

testURL = "http://homepages.inf.ed.ac.uk/wadler/testpage.html"

testHTML :: String
testHTML =
  "<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 3.2//EN\">\n"
    ++ "<html>\n"
    ++ "<head>\n"
    ++ "<title>FP: Tutorial 4</title>\n"
    ++ "</head>\n"
    ++ "<body>\n"
    ++ "<h1>A Boring test page</h1>\n"
    ++ "<h2>for tutorial 4</h2>\n"
    ++ "<a href=\"https://course.inf.ed.ac.uk/inf1a\">Inf1A Learn</a><br>\n"
    ++ "<b>Lecturer:</b> <a href=\"mailto:wadler@inf.ed.ac.uk\">Philip Wadler</a><br>\n"
    ++ "<b>TA:</b> <a href=\"mailto:cchirita@exseed.ed.ac.uk\">Claudia-Elena Chirita</a>\n"
    ++ "</body>\n"
    ++ "</html>\n\n"

testLinks :: [Link]
testLinks =
  [ "https://course.inf.ed.ac.uk/inf1a\">Inf1A Learn",
    "mailto:wadler@inf.ed.ac.uk\">Philip Wadler",
    "mailto:cchirita@exseed.ed.ac.uk\">Claudia-Elena Chirita"
  ]

testAddrBook :: [(Name, Email)]
testAddrBook =
  [ ("Philip Wadler", "wadler@inf.ed.ac.uk"),
    ("Claudia-Elena Chirita", "cchirita@exseed.ed.ac.uk")
  ]

-- </sample data>
-- <system interaction>

getURL :: String -> IO String
getURL url = simpleHTTP (getRequest url) >>= getResponseBody

emailsFromURL :: URL -> IO ()
emailsFromURL url =
  do
    html <- getURL url
    let emails = (emailsFromHTML html)
    putStr (ppAddrBook emails)

emailsByNameFromURL :: URL -> Name -> IO ()
emailsByNameFromURL url name =
  do
    html <- getURL url
    let emails = (emailsByNameFromHTML html name)
    putStr (ppAddrBook emails)

-- </system interaction>
-- <exercises>

-- 1.
sameString :: String -> String -> Bool
sameString alist blist
  | length alist /= length blist = False
  | otherwise = and [toLower (fst x) == toLower (snd x) | x <- zip alist blist]

-- 2.
prefix :: String -> String -> Bool
prefix pre str = sameString pre (take (length pre) str)

prop_prefix_pos :: String -> Int -> Property
prop_prefix_pos str n = n >= 0 ==> prefix substr (map toUpper str)
  where
    substr = take n str

prop_prefix_neg :: String -> Int -> Property
prop_prefix_neg str n = 0 <= n && n < length str ==> (not $ prefix str substr)
  where
    substr = take n str

-- 3.
contains :: String -> String -> Bool
contains _ "" = True
contains "" _ = False
contains str substr = or [prefix substr (drop x str) | x <- [0 .. length str]]

prop_contains :: String -> Int -> Int -> Property
prop_contains str n m = 0 <= n && n < length str ==> (not $ contains str substr)
  where
    substr = take n str

-- fix later

-- 4.
takeUntil :: String -> String -> String
takeUntil "" str = str
takeUntil _ "" = ""
takeUntil substr str
  | prefix substr str = ""
  | otherwise = take 1 str ++ takeUntil substr (drop 1 str)

dropUntil :: String -> String -> String
dropUntil "" str = str
dropUntil _ "" = ""
dropUntil substr str
  | prefix substr str = drop (length substr) str
  | otherwise = dropUntil substr (drop 1 str)

-- 5.
split :: String -> String -> [String]
split "" _ = error "empty list as first parameter"
split _ "" = []
split sep str
  | contains str sep = [takeUntil sep str] ++ split sep (dropUntil sep str)
  | otherwise = [str]

reconstruct :: String -> [String] -> String
reconstruct "" _ = error "empty list as first parameter"
reconstruct _ [] = []
reconstruct sep xs
  | length xs == 1 = xs !! 0
  | otherwise = xs !! 0 ++ sep ++ reconstruct sep (drop 1 xs)

prop_split :: String -> String -> Property
prop_split sep str = not (null sep) ==> reconstruct sep (split sep str) `sameString` str

-- 6.
linksFromHTML :: HTML -> [Link]
linksFromHTML html
  | contains html "</a>" = [dropUntil "<a href=\"" (takeUntil "</a>" html)] ++ linksFromHTML (dropUntil "</a>" html)
  | otherwise = [dropUntil "<a href=\"" (takeUntil "</a>" html)]

testLinksFromHTML :: Bool
testLinksFromHTML = linksFromHTML testHTML == testLinks

-- 7.
takeEmails :: [Link] -> [Link]
takeEmails links = [link | link <- links, prefix "mailto:" link]

-- 8.
link2pair :: Link -> (Name, Email)
link2pair link = (takeUntil "\">" (drop (length "mailto:") link), dropUntil "\">" link)

-- 9.
emailsFromHTML :: HTML -> [(Name, Email)]
emailsFromHTML html = [link2pair link | link <- links]
  where
    links = takeEmails (linksFromHTML html)

testEmailsFromHTML :: Bool
testEmailsFromHTML = emailsFromHTML testHTML == testAddrBook

-- Optional Material

-- 10.
findEmail :: Name -> [(Name, Email)] -> [(Name, Email)]
findEmail name tuple = undefined

-- 11.
emailsByNameFromHTML :: HTML -> Name -> [(Name, Email)]
emailsByNameFromHTML = undefined

ppAddrBook :: [(Name, Email)] -> String
ppAddrBook addr = unlines [name ++ ": " ++ email | (name, email) <- addr]
