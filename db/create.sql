CREATE TABLE public.poll
(
  id bigint,
  title character varying,
  "desc" character varying,
  start timestamp with time zone,
  "end" timestamp with time zone,
  owner bigint,
  "channel_id" bigint
)
WITH (
  OIDS=FALSE
);
ALTER TABLE public.poll
  OWNER TO pollock;

CREATE TABLE public.channel
(
  id bigint,
  name character varying
)
WITH (
  OIDS=FALSE
);
ALTER TABLE public.channel
  OWNER TO pollock;

