open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type repository_name = { nameWithOwner : string } [@@deriving yojson]
type repository = { url : string; nameWithOwner : string } [@@deriving yojson]

type issue = {
  url : string;
  title : string;
  body : string;
  repository : repository_name;
}
[@@deriving yojson]

type issueContribution = { occurredAt : string; issue : issue }
[@@deriving yojson]

type issueContributions = { nodes : issueContribution list } [@@deriving yojson]

type pullRequest = {
  url : string;
  title : string;
  body : string;
  repository : repository_name;
}
[@@deriving yojson]

type pullRequestContribution = {
  occurredAt : string;
  pullRequest : pullRequest;
}
[@@deriving yojson]

type pullRequestContributions = { nodes : pullRequestContribution list }
[@@deriving yojson]

type pullRequest_title = { title : string } [@@deriving yojson]

type pullRequestReview = {
  url : string;
  pullRequest : pullRequest_title;
  body : string;
  state : string;
  repository : repository_name;
}
[@@deriving yojson]

type pullRequestReviewContribution = {
  occurredAt : string;
  pullRequestReview : pullRequestReview;
}
[@@deriving yojson]

type pullRequestReviewContributions = {
  nodes : pullRequestReviewContribution list;
}
[@@deriving yojson]

type repositoryContribution = { occurredAt : string; repository : repository }
[@@deriving yojson]

type repositoryContributions = { nodes : repositoryContribution list }
[@@deriving yojson]

type contributionsCollection = {
  issueContributions : issueContributions;
  pullRequestContributions : pullRequestContributions;
  pullRequestReviewContributions : pullRequestReviewContributions;
  repositoryContributions : repositoryContributions;
}
[@@deriving yojson]

type issue_title = { title : string } [@@deriving yojson]

type issueComment = {
  url : string;
  publishedAt : string;
  issue : issue_title;
  repository : repository_name;
  body : string;
}
[@@deriving yojson]

type issueComments = { nodes : issueComment list } [@@deriving yojson]

type user_data = {
  login : string;
  contributionsCollection : contributionsCollection;
  issueComments : issueComments;
}
[@@deriving yojson]

type data = {
  user : user_data option; [@yojson.option]
  viewer : user_data option; [@yojson.option]
}
[@@deriving yojson]

type t = { data : data } [@@deriving yojson]
