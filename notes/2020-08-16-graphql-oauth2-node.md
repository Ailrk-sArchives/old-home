-- tag note CORS GraphQL node OAuth2
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


#### How GraphQL words.
##### Type system and query language
The query language sent from the request side match exactly the type of the data on the server side, thus you can predict what shape of the data you will get. Because GraphQL is designed to be language agnostic, it has a stand alone type language to describe it's schema.

```
type Starship {
    id: ID!
    name: String!
    length(unit: LengthUnit = METER): Float
}

"""
schema declare some entry point types, namely query and mutation.
Here it defines a query type Query and a mutation type Mutation.
"""
schema {
    query: Query
    mutation Mutation
}

"""
Define the Query type mentioned in schema.
Query and Mutation are the same as any other graphQL object
types, the only difference is they are the entry point into
the schema
"""
type Query {
    hero(episode: Episode): Character
    droid(id: ID!): Droid
}
```

##### Validation
The type system will catch incorrectly query ahead of time, and free you from the hell of runtime checking.

##### Execution
After the query is validated, it will be executed by GraphQL server, which will return a result that mirrors the shape of the query. Each field in a graphql query is backed by a function called resovler on the server side. The query terminate when it reach scalar only. If a field is object, GraphQL will search value for fields of the object until only scalar remains.

##### Root fields and Resolers
The `root type` of a graphql server has a type that represent all of the possible entry points the api provides. The resolver function takes four arguments:

- obj: parent obj
- args: argument provided to the field in query
- conetxt: context get passed to every resolver. contain some states.
- info: field specific information.

A more complete example

```
"""
define type first, all the resovlers will be defined base on this
framework
"""
type Query {
    human(id: ID!): Human
}

type Human {
    name: String
    appearsIn: [Episode]
    starships: [Starship]
}

enum Episode {
    NEWHOPE
    EMPIRE
    JEDI
}

type Starship {
    name: String
}

"""
JS example.
The root type provides a field called `human`. The function is a
resolver defined by user.

The resolver returns a promise, which makes it a asynchronous
function.
"""

Query: {
    human(obj, args, context, info) {
        return context.db.loadHumanById(args.id).then(
            userData => new Human(userData))
    }
}

"""
Now define a trivial resolver. Each fields are backed by a specific
trivial resolver.  The resovler below is very simple, graphql can
derive the implementation even if it is not provided.
"""

Human: {
    name(obj, args, context, info) {
        return obj.name
    }
}

"""
List resolver for human.
GraphQL will call Promise.all() on list of promise before continuing
"""
Human: {
    starships(obj, args, context, info) {
        return obj.starshipIDs.map(
            id => context.db.loadStarship.ById(id).then(
                shipData => new Starship(shipData)
            )
        )
    }
}
```

Note resolver for enum will return an integer, and the type system is able to find the right enum value from it.

##### Introspection
GraphQL support querying for meta data from the GraphQL server to know type of data is available. It's called introsepction system.

Some predefined fields for introspection system. You can use it for GraphQL.

- `__schema`
- `__type`
- `__typekind`
- `__field`
- `__inputvalue`
- `__enumvalue`
- `__directive`

An example of querying with `__schema` and `__type`

```
""" return query types available """
{
    __schema {
        queryType {
            name
        }
    }
}

""" kind return the data type """
{
    __type(name: "Droid") {
        name
        kind
    }
}
```


#### Some tips
##### HTTP & JSON
GraphQL servs over HTTP with single endpoint, as opposed to the multiple endpoint approach of RESTful api.

To achieve better efficiency it's better to add `Accept-Encoding: gzip` on each response header.

##### Authorization
Authorization should totally be separated from graph ql. It's better for resolver to not be aware of authorization at all. Same energy as decoupling stuffs principle.

#### GraphQL arch design: Single source of truth
Everything pass through the same validation, authorization and error handling rules.
```
---------------------------------------------------------
            REST    GraphQL    RPC
---------------------------------------------------------
                  Authorization
        Business Logic Layer (single source of truth)
---------------------------------------------------------
                Persistence Layer
---------------------------------------------------------
```

## OAuth2.0
OAuth2 authorization framework enables a third party application to obtain limited access to an HTTP service. Good opportunity to learn how they define application level protocals.

In a naive authoization approach, the client might use the resouce owner's credential directly to access resouce. For example, a client store the password and post it in each request. Because client is using the full credential directly, the resouce owner has little control over how resources can be accessd.

OAuth2 introduce new authorization layer between, and separate the role of client from resource owner. The client request access of resources owned by the resource owner but hosed by resource server. The client first request access token with corresponding credential, and get the access token, which is a credential issued by the resource owner that allows the client to access resouce from the data server.

#### Roles
- `Resource owner`: Granting access to protected resource.
- `Resource server`: Hosting the protected resource. Response access request with access token.
- `Client`: Apllications that request protected data with `resource owner`'s credentials.
- `Authorization server`: The server issuing access token after authenticating the `resource owner`.

`Authorization server` and `Response server` might be the same server or not, it's all depend on implementation.

#### Work Flow
```
            → 1. Authorization request → |Resource Owner
 Client     ← 2. Authorization Grant  ←  |

            → 3. Authorization Grant   → |Authorization Server
            ← 4. Access token          ← |

            → 5. Access Token          → |Resource Server
            ← 6. Protected Resource    ← |
```
Step 1, 2 can be done with a traditional password approach. But with the password client only get an access token, that contains some constraint information about the access. The actual resource need to be obtained with the access token and will be invalid after the token is expired.

#### Authorization Grant
Authorization grant is the credential used by the client to gain access token from the authorization server. there are several types of authorization grants defined in the rfc

##### Authorization code
Authorization code is obtained by using an authorized server as an intermediary between the client and the resource owner. The client will not ask for the credential from the resource owner directly. Instead, it will redirect the credential to the authorization server. This way the client will not know anything about the owner's credential.

##### Implicit
It's a simplified authorization code flow. The goal is to optmize for in browser clients. With the implicit flow, instead of issuing the client a access token, the client will issue an access token directly since the resource owner is already using the client directly.

#### Acess Token
Access tokens are credentials used to access protected data from the resource server. Access token itself is a string, normally get hashed, represents the access right granted by the authorization server (which is granted the right to issue the token by authorization grant from the resource owner passed by the client). Token can represent a specifc scopes and durations of access granted by the resource owner.

Access token provides another layer of abstraction which replace diffrerent credential with a single token undertodd by the resource server.

#### Refresh Token
Refresh token are another type of credential that be used to obtain access token. It can be used by the client to exchange a new access token when the previous one is expired.

#### Conclusion
OAuth2.0 is a very good example of layering. We can always introduce a new layer of abstraction for data to be further processed before it get passed to the next layer. By doing so we can decouple the responsibility of different layers, and add new logic in between.


## CORS
Historically browers has this `same origin policy`, you cannot access resources that is not hosted by your own server. CORS is a mechanism to relax this restriction.

Cross-origin resouce sharing. A mechanism to restrict types of resources can be request from a web page from another domain outside the domain where the first request is served. Some resources like images, css, scripts, videos and able to be requested in a CORS way, but things like ajax request is forbidden.

#### Request Headers
- `Origin`
- `Access-Control-Request-Method` specify methods used for cross site access
- `Access-Control-Request-Header`

#### Response Headers
- `Access-Control-Allow-Origin` white list
- `Access-Control-Allow-Methods` methods allowed for corss site request.

#### Things to take care about
If you care about the security, `Access-Control-Allow-Origin` should not be `*`. This allows any applications to access your resource. I used to use the config for my falsk app, but you really shouldn't do that. `Access-Control-Allow-Methods` put restriction on types of HTTP methods cross site access is allowed, some methods only used internally can be turned off.
