open Ppx_yojson_conv_lib.Yojson_conv.Primitives

module Repository = struct
  type t = { url : string; nameWithOwner : string } [@@deriving yojson]
  type name = { nameWithOwner : string } [@@deriving yojson]

  type contribution = { occurredAt : string; repository : t }
  [@@deriving yojson]

  type contributions = { nodes : contribution list } [@@deriving yojson]
end

module Issue = struct
  type t = {
    url : string;
    title : string;
    body : string;
    repository : Repository.name;
  }
  [@@deriving yojson]

  type title = { title : string } [@@deriving yojson]
  type contribution = { occurredAt : string; issue : t } [@@deriving yojson]
  type contributions = { nodes : contribution list } [@@deriving yojson]

  type comment = {
    url : string;
    publishedAt : string;
    issue : title;
    repository : Repository.name;
    body : string;
  }
  [@@deriving yojson]

  type comments = { nodes : comment list } [@@deriving yojson]
end

module PullRequest = struct
  type t = {
    url : string;
    title : string;
    body : string;
    repository : Repository.name;
  }
  [@@deriving yojson]

  type title = { title : string } [@@deriving yojson]

  type contribution = { occurredAt : string; pullRequest : t }
  [@@deriving yojson]

  type contributions = { nodes : contribution list } [@@deriving yojson]

  module Review = struct
    type t = {
      url : string;
      pullRequest : title;
      body : string;
      state : string;
      repository : Repository.name;
    }
    [@@deriving yojson]

    type contribution = { occurredAt : string; pullRequestReview : t }
    [@@deriving yojson]

    type contributions = { nodes : contribution list } [@@deriving yojson]
  end
end

type contributionsCollection = {
  issueContributions : Issue.contributions;
  pullRequestContributions : PullRequest.contributions;
  pullRequestReviewContributions : PullRequest.Review.contributions;
  repositoryContributions : Repository.contributions;
}
[@@deriving yojson]

type user_data = {
  login : string;
  contributionsCollection : contributionsCollection;
  issueComments : Issue.comments;
}
[@@deriving yojson]

type data = {
  user : user_data option; [@yojson.option]
  viewer : user_data option; [@yojson.option]
}
[@@deriving yojson]
(** The key is either [viewer] or [user] depending on the request but the value associated is the same. *)

type t = { data : data } [@@deriving yojson]
