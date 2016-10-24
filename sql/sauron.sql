create table config (
	key				varchar(256) primary key,
	value				varchar(256)
);

insert into config (key, value) values
('ADD-BLACK',          'true'),
('ACTIVE-SAURON',       NULL),
('BIRD-PROTOCOL',       'sauron'),
('BIRD-RELOAD',         'sudo /etc/init.d/bird reload'),
('BLOCK-URL',           'http://shtraf.net.ru/sauron/block/'),
('CHECK-PERIOD',        '60'),
('DEL-BLACK',           'true'),
('DOWNLOAD-PERIOD',     '23'),
('DUMP-FORMAT',         '2.2'),
('HOME-DIRECTORY',      '/home/sauron/'),
('LIST-BLACK',          'true'),
('NGINX-PORT',          '8080'),
('NGINX-RELOAD',        'sudo /etc/init.d/nginx reload'),
('NGINX-RESOLVER',      '127.0.0.1'),
('ROOT-MEANS-DOMAIN',   't'),
('ROUTERS',             ''),
('STORE-INTERVAL',      '7'),
('WORKING-REGISTRY-ID', NULL);


create sequence last_info_seq
	start 1
	increment 1;

create table last_info (
	id				integer primary key
					default nextval('last_info_seq'),
	time				timestamp with time zone
					default now(),
	_date				bigint,
	_urgently			bigint,
	_web_service			varchar(32),
	_dump_format			varchar(32),
	_doc				varchar(32)
);

create index last_info_time_idx on last_info(time);


create sequence request_seq
	start 1
	increment 1;

create table request (
	id				integer primary key
					default nextval('request_seq'),
	time				timestamp with time zone
					default now(),
	_result				boolean,
	_comment			varchar(1024),
	_code				varchar(1024)
);

create index request_time_idx on request(time);
create index request_code_idx on request(_code);


create sequence registry_seq
	start 1
	increment 1;

create table registry (
	id				integer primary key
					default nextval('registry_seq'),
	request_id			integer
					references request(id),
	_time				timestamp with time zone not NULL
					default now(),
	_update_time			timestamp with time zone
					default now() - interval '1 hour',
	_update_time_urgently		timestamp with time zone
					default now() - interval '1 hour',
	_format_version			varchar(16),
	completed			boolean,
	resource_count			integer
);

create index registry_time_idx on registry(_time);
create index registry_update_time on registry(_update_time);


create table entry_type (
	id				integer primary key,
	name				varchar(128)
);

insert into entry_type (id, name) values (1, 'реестр ЕАИС');
insert into entry_type (id, name) values (2, 'реестр НАП');
insert into entry_type (id, name) values (3, 'реестр 398-ФЗ');
insert into entry_type (id, name) values (4, 'реестр 97-ФЗ (организаторы распространения информации)');
insert into entry_type (id, name) values (5, 'реестр НАП, постоянная блокировка сайтов');
insert into entry_type (id, name) values (6, 'реестр нарушителей прав субъектов персональных данных');


create sequence content_seq
	start 1
	increment 1;

create table content (
	id				integer primary key
					default nextval('content_seq'),
	registry_id			integer not NULL
					references registry(id),
	_id				integer,
	_include_time			timestamp not NULL,
	_urgency			boolean,
	_entry_type_id			integer not NULL
					references entry_type(id),
	_block_domain			boolean,
	_decision_date			date,
	_decision_number		varchar(256),
	_decision_org			varchar(1024)
);

create index content_registry_idx on content(registry_id);
create index content_include_time_idx on content(_include_time);


create sequence resource_seq
	start 1
	increment 1;

create table resource (
	id				integer primary key
					default nextval('resource_seq'),
	registry_id			integer not NULL
					references registry(id),
	content_id			integer not NULL
					references content(id),
	ssl				boolean,
	_domain				varchar(1024),
	_location			varchar(4096),
	_query				varchar(4096),
	_anchor				varchar(4096),
	_url				varchar(4096)
);

create index resource_registry_idx on resource(registry_id);
create index resource_content_idx on resource(content_id);


create sequence rkn_ip_address_seq
	start 1
	increment 1;

create table rkn_ip_address (
	id				integer primary key
					default nextval('rkn_ip_address_seq'),
	registry_id			integer not NULL
					references registry(id),
	content_id			integer not NULL
					references content(id),
	_address			inet,
	_subnet				boolean
);

create index rkn_ip_address_registry_idx on rkn_ip_address(registry_id);
create index rkn_ip_address_content_idx on rkn_ip_address(content_id);


create sequence realm_seq
	start 1
	increment 1;

create table realm (
	id				integer primary key
					default nextval('realm_seq'),
	name				varchar(256),
	block_url			varchar(1024),
	active				boolean
);


create sequence black_time_seq
	start 1
	increment 1;

create table black_time (
	id				integer primary key
					default nextval('black_time_seq'),
	realm_id			integer not NULL
					references realm(id),
	wday				smallint,
	start				time,
	stop				time
);

create index black_time_realm_idx on black_time(realm_id);


create sequence realm_internal_address_seq
	start 1
	increment 1;

create table realm_internal_address (
	id				integer primary key
					default nextval('realm_internal_address_seq'),
	realm_id			integer not NULL
					references realm(id),
	address				inet
);

create index realm_internal_address_realm_idx on realm_internal_address(realm_id);


create sequence realm_external_address_seq
	start 1
	increment 1;

create table realm_external_address (
	id				integer primary key
					default nextval('realm_external_address_seq'),
	realm_id			integer not NULL
					references realm(id),
	address				inet
);

create index realm_external_address_realm_idx on realm_external_address(realm_id);


create sequence black_list_seq
	start 1
	increment 1;

create table black_list (
	id				integer primary key
					default nextval('black_list_seq'),
	realm_id			integer not NULL
					references realm(id),
	domain				varchar(1024)
);

create index black_list_realm_idx on black_list(realm_id);

----
