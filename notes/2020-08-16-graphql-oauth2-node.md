-- tag note haskell xmonad zipper data-structure
-- title GraphQL, oauth2, node, all web stuffs.
-- date 2020-08-16
-- source https://graphql.org/learn/thinking-in-graphs/
;;
# GraphQL, oauth2, node, all web stuffs.
Some graphql, oauth2 to prepare for making canvas bot.


## GraphQL
Replcement for RESTful apis. It's a query language with a sound type system for APIs and a server side runtime to exeucting the query. It's a facade design and can be added to existed code and data.

GraphQL modeling data with graph, you might try some well known technics like zipper to work with it.

#### Process
You model your data with a "type system". (I feel they just want to use this word) and define some functions for every single fields
```
type Query {
    me: User
}
type User {
    id: ID
    name: String
}
functions Query_me(request) { return request.auth.user; }
```

And when the graphsQL server is running, query works like this:

```
{ me { name } } ->  { "me": { "name": "Luke Skywalker"}}
```


#### How to use
The whole design is like a weirdly typed quasi quotes for describing json. You have some short hand to work with the tree easier, by largely it's just a mega tempalte string. I can imagine support this in typescript directly via bable transform.

##### Fields
Note Your query has exactly the same shape as your result.

Note Data returned can be nested elements like another object.

```
{ hero {name} } -> { "data": { "hero": { "name": "R2-D2" } } }
```

##### Arugments
Each fileds can be passed with an argument for filtering. The following example with query for the specific human with id "1000".
```
{
    human(id: "1000") {
        name
        height
    }
}
```

##### Alias
You can make new alias of the same field
```
{
    empireHero: hero(episode: EMPIRE) { name }
    jediHero: hero(episode: JEDI) { name }
}
```

##### Fragments
Alias for a sub tree. It works like quasi quotes you can replace text inside.
```
query com($first: int = 3) {
    com1: hero(episode: EMPIRE) { ...comparisionField }
    com2: hero(episode: JEDI) { ...comparisionField }
}
fragment comparisionField on Character {
    name
    friendConnection(first: $first) {
        totalCount
        edges {
            node {
                name
            }
        }
    }
}
```
##### Derectives
Some macro for conditional querying. You have `@include(if: Boolean)` and `@skip(if: Boolean)` as builtin which are exactly the negation of each other.

```
query Hero($episode: Episode, $withFriends: Boolean!) {
    hero(episode: $episode) {
        name
        friends @include(if: $withFriends) {
            name
        }
    }
}
```

Result
```
{
    "data": {
        "hero" : { "name" : "R2-D2" }
    }
}
```

##### Mutations
Use mutation request to mutate value on the server side.
```
mutation CreateReviewForEpisode($ep: Episode!, $review: ReviewInput!) {
  createReview(episode: $ep, review: $review) {
    stars
    commentary
  }
}
```
The data writes into the quasi quote will get transferred and recorded on the server side.


#### GraphQL arch design: Single source of truth
Everything pass through the same validation, authorization and error handling rules.
```
-------------------------------------
    REST    GraphQL    RPC
-------------------------------------
         Authorization
        Business Logic Layer
-------------------------------------
         Persistence Layer
-------------------------------------
```
