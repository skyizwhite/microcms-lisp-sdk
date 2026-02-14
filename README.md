# microcms-lisp-sdk

microcms-lisp-sdk is a Common Lisp SDK for interacting with [microCMS](https://microcms.io) via its REST API. It provides macros to define client functions for both list and object type endpoints.

## Configuration

Before making API requests, set your API key and service domain:

```lisp
(setf microcms:*api-key* "your-api-key")
(setf microcms:*service-domain* "your-service-domain") ; e.g., "example" for example.microcms.io
```

## Usage

### List Type Endpoint

`query` and `content` must be provided as property list (plist), with keys written in kebab-case (e.g., `:draft-key`).
The JSON response from the microCMS API are automatically converted into plist, with keys transformed from camelCase to kebab-case.

| Function Name | Arguments | Description |
|---------------|-----------|-------------|
| `get-list` | (`endpoint` &key `query`) | Get a list of articles. |
| `get-item` | (`endpoint` `id`, &key `query`) | Get details of a specific article by ID. |
| `create-item` | (`endpoint` `content`, &key `query`) | Create a new article with the given content. |
| `update-item` | (`endpoint` `id`, `content`) | Update an existing article by its ID with new content. |
| `delete-item` | (`endpoint` `id`) | Delete an article by its ID. |

### Object Type Endpoint

| Function Name | Arguments | Description |
|---------------|-----------|-------------|
| `get-object` | (`endpoint` &key `query`) | Retrieve the profile object. |
| `update-object` | (`endpoint` `content`) | Update the content of the profile object. |

### License

MIT License
© 2025 Akira Tempaku
