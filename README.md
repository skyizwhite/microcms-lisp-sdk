# microcms-lisp-sdk

microcms-lisp-sdk is a Common Lisp SDK for interacting with [microCMS](https://microcms.io) via its REST API. It provides macros to define client functions for both list and object type endpoints.

## ⚙️ Configuration

Before making API requests, set your API key and service domain:

```lisp
(setf microcms:*api-key* "your-api-key")
(setf microcms:*service-domain* "your-service-domain") ; e.g., "example" for example.microcms.io
```

## 🚀 Usage

### List Type Endpoint

Use `define-list-client` macro to define functions for list-type content.

`query` and `content` must be provided as property list (plist), with keys written in kebab-case (e.g., `:draft-key`).
The JSON response from the microCMS API are automatically converted into plist, with keys transformed from camelCase to kebab-case.

```lisp
(microcms:define-list-client article)
```
This will generate the following functions:

| Function Name | Arguments | Description |
|---------------|-----------|-------------|
| `get-article-list` | (&key `query`) | Get a list of articles. |
| `get-article-detail` | (`id`, &key `query`) | Get details of a specific article by ID. |
| `create-article` | (`content`, &key `query`) | Create a new article with the given content. |
| `update-article` | (`id`, `content`) | Update an existing article by its ID with new content. |
| `delete-article` | (`id`) | Delete an article by its ID. |

### Object Type Endpoint

Use `define-object-client` macro to define functions for object-type content.

```lisp
(microcms:define-object-client profile)
```

This will generate the following functions:

| Function Name | Arguments | Description |
|---------------|-----------|-------------|
| `get-profile-object` | (&key `query`) | Retrieve the profile object. |
| `update-profile` | (`content`) | Update the content of the profile object. |

### 📄 License

MIT License
© 2025 Akira Tempaku
