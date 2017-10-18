alter table repos
drop constraint repos_kind_type_url_key,
drop column kind,
drop column type,
alter repo_kind_id set not null,
alter repo_type_id set not null,
add unique (repo_kind_id, repo_type_id, url)
