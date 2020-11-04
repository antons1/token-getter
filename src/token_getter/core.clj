(ns token-getter.core
  (:require [clj-http.client :as http]
            [clojure.data.json :as json]
            [clojure.string :as string])
  (:import (java.util Base64))
  (:gen-class))

(defonce auth-tokens (atom nil))

(defn auth-token-url []
  "")

(defn valid-token? [{:keys [expires-at]}]
  (and expires-at
       (> expires-at
          (System/currentTimeMillis))))

(defn store-tokens-from-api-response
  "Take API response from telia-indentity, and store it in `auth-tokens`"
  [{:keys [access_token expires_in refresh_token refresh_expires_in]} received]
  (reset! auth-tokens
          {:access  {:token      access_token
                     :expires-at (+ received (* expires_in 1000))}
           :refresh {:token      refresh_token
                     :expires-at (+ received (* refresh_expires_in 1000))}}))

(defn fetch-tokens
  "Fetches access token from identity
  grant-type = nil => client_credentials (client id/secret)
  grant-type = :refresh_token => refresh_token (id/secret + valid refresh token supplied in token)
  grant-type = :authorization_code => auhtorization_code (id/secret + code from identity login in browser supplied in token + offboarding-id for redirect_uri"
  ([client-id client-secret]
   (fetch-tokens client-id client-secret nil nil))
  ([client-id client-secret grant-type token]
   (-> (http/post
         (auth-token-url)
         {;:debug       true, :debug-body true,
          ;:save-request? true
          :accept      :json
          :form-params (merge {:client_id     client-id
                               :client_secret client-secret}
                              (case grant-type
                                :authorization_code {:code         token
                                                     :grant_type   "authorization_code"
                                                     :redirect_uri (str "http://localhost")}
                                :refresh_token {:refresh_token token
                                                :grant_type    "refresh_token"}
                                {:grant_type "client_credentials"}))})
       :body
       (json/read-str :key-fn keyword))))

(defn auth-token
  "Get access token from telia-identity, refreshing or fetching a new one if the stored one is invalid"
  [client-id client-secret]
  (let [request-time (System/currentTimeMillis)]
    (cond (valid-token? (:access @auth-tokens)) (-> @auth-tokens :access :token)
          (valid-token? (:refresh @auth-tokens)) (-> (fetch-tokens client-id client-secret :refresh_token (-> @auth-tokens :refresh :token))
                                                     (store-tokens-from-api-response request-time)
                                                     :access :token)
          :else (-> (fetch-tokens client-id client-secret)
                    (store-tokens-from-api-response request-time)
                    :access :token))))

(defn base64-decode
  "Utility function over the Java 8 base64 decoder"
  [to-decode]
  (String. (.decode (Base64/getDecoder) ^String to-decode)))

(defn decoded-jwt
  "Transform a properly formed JWT into a Clojure map"
  [jwt]
  (when-let [jwt-parts (string/split jwt #"\.")]
    (when (= 3 (count jwt-parts))
      (let [[b64-header b64-payload b64-signature] jwt-parts]
        {:header    (json/read-str (base64-decode b64-header) :key-fn keyword)
         :payload   (json/read-str (base64-decode b64-payload) :key-fn keyword)
         :signature b64-signature}))))



(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (prn (auth-token (or (first args) "") (or (second args) ""))))
