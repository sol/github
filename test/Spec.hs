{-# LANGUAGE OverloadedStrings #-}
module Spec (main, spec) where

import           Test.Hspec.ShouldBe
import           Test.HUnit.ShouldBe.Contrib

import           Github.Repos
import           Data.String
import           Data.ByteString.Char8 as B
import           Github.Private
import           Control.Exception

main :: IO ()
main = hspecX spec

-- a test user
-- https://github.com/s1TEPIn5
-- s1TEPIn5@mailinator.com
user = "s1TEPIn5"
pass = "4Ex6at0I"
auth = (B.pack user, B.pack pass) :: BasicAuth

spec :: Specs
spec = do

  describe "doHttps (an internal function)" $ do
    it "can do basic requests" $ do
      let url = "https://github.com/"
      r <- doHttps "GET" url Nothing Nothing
      r `shouldSatisfy` isRight

    it "returns Left on 404" $ do
      let url = "https://github.com/foo/bar/baz"
      r <- doHttps "GET" url Nothing Nothing
      r `shouldSatisfy` isLeft

  describe "createRepo" $ do

    let repo = "test-create"

    it "can create a repo" $ do
      Right r <- createRepo auth (newRepo repo) {newRepoHasIssues = Just False}
      repoName r `shouldBe` repo
      repoHasIssues r `shouldBe` Just False
      `finally` (deleteRepo auth user repo)

  describe "editRepo" $ do

    let repo = "test-edit"
    
    it "can set a new description for a repo" $ do
      let newDescription = Just "some description"
      Right r <- createRepo auth (newRepo repo)
      repoDescription r `shouldSatisfy` (/= newDescription)
      Right r <- editRepo auth user repo def {editDescription = newDescription}
      repoDescription r `shouldBe` newDescription
      `finally` (deleteRepo auth user repo)

  describe "delete" $ do
    it "deletes a repository" $ do
      let repo = "test-delete"
      Right r <- createRepo auth (newRepo repo)
      r <- deleteRepo auth user repo
      r `shouldSatisfy` isRight
      Right repos <- userRepos user All
      repos `shouldBe` []

    it "fails, if repository does not exist" $ do
      let repo = "test-delete"
      r <- deleteRepo auth user repo
      r `shouldSatisfy` isLeft
