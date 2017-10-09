update repos
set
  repo_kind_id = (
    select repo_kinds.id
    from repo_kinds
    where repo_kinds.content = repos.kind
  ),
  repo_type_id = (
    select repo_types.id
    from repo_types
    where repo_types.content = repos.type
  )
