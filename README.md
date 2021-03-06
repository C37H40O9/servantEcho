# Servant echo app

Simple show-case project that just echo incoming messages with backend on Free, Church Encoded Free and freer monads.

Include:
- tests
- benchmarks
- swagger & servant-docs api documentation generators.


## Usage
In order to choose between different backend types the **BACKEND** variable should be set to:
- FM -> free monad (default)
- CEFM -> church-encoded free monad
- FR -> freer monad


Example of servant-docs(http://localhost:8080/servant-docs.md):

## GET /echo/:message

### Captures:

- *message*: message to echo

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
"Text sample"
```



Example of swagger (http://localhost:8080/swagger.json):

```json
{
   "swagger":"2.0",
   "info":{
      "version":"1.0",
      "title":"Example API",
      "description":"This is an API that tests swagger integration"
   },
   "paths":{
      "/echo/{message}":{
         "get":{
            "produces":[
               "application/json;charset=utf-8"
            ],
            "parameters":[
               {
                  "required":true,
                  "in":"path",
                  "name":"message",
                  "type":"string"
               }
            ],
            "responses":{
               "404":{
                  "description":"`message` not found"
               },
               "200":{
                  "schema":{
                     "type":"string"
                  },
                  "description":""
               }
            }
         }
      }
   }
}
```
