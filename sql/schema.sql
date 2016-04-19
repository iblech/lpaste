--
-- PostgreSQL database dump
--

SET statement_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;

--
-- Name: plpgsql; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;


--
-- Name: EXTENSION plpgsql; Type: COMMENT; Schema: -; Owner: -
--

COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';


--
-- Name: uuid-ossp; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS "uuid-ossp" WITH SCHEMA public;


--
-- Name: EXTENSION "uuid-ossp"; Type: COMMENT; Schema: -; Owner: -
--

COMMENT ON EXTENSION "uuid-ossp" IS 'generate universally unique identifiers (UUIDs)';


SET search_path = public, pg_catalog;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: channel; Type: TABLE; Schema: public; Owner: -; Tablespace:
--

CREATE TABLE channel (
    id integer NOT NULL,
    title character varying(28) NOT NULL
);


--
-- Name: channel_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE channel_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: channel_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE channel_id_seq OWNED BY channel.id;


--
-- Name: hint; Type: TABLE; Schema: public; Owner: -; Tablespace:
--

CREATE TABLE hint (
    id integer NOT NULL,
    paste bigint,
    type text NOT NULL,
    content text NOT NULL
);


--
-- Name: hint_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE hint_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: hint_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE hint_id_seq OWNED BY hint.id;


--
-- Name: language; Type: TABLE; Schema: public; Owner: -; Tablespace:
--

CREATE TABLE language (
    id integer NOT NULL,
    name character varying(32) NOT NULL,
    title character varying(64) NOT NULL,
    ordinal integer DEFAULT 0 NOT NULL,
    visible boolean DEFAULT false NOT NULL
);


--
-- Name: language_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE language_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: language_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE language_id_seq OWNED BY language.id;


--
-- Name: paste; Type: TABLE; Schema: public; Owner: -; Tablespace:
--

CREATE TABLE paste (
    id bigint NOT NULL,
    title character varying(512) NOT NULL,
    content text NOT NULL,
    tags text,
    author character varying(128) NOT NULL,
    created timestamp with time zone DEFAULT now() NOT NULL,
    views integer DEFAULT 0 NOT NULL,
    language integer,
    channel integer,
    annotation_of bigint,
    expire timestamp with time zone,
    output text,
    public boolean DEFAULT true,
    revision_of bigint,
    spamrating decimal DEFAULT 0
);


--
-- Name: paste_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE paste_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: paste_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE paste_id_seq OWNED BY paste.id;


--
-- Name: report; Type: TABLE; Schema: public; Owner: -; Tablespace:
--

CREATE TABLE report (
    id bigint NOT NULL,
    paste bigint NOT NULL,
    comments text NOT NULL,
    created timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: private_paste; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW private_paste AS
    SELECT paste.id, paste.title, paste.content, paste.author, paste.created, paste.views, paste.language, paste.channel, paste.annotation_of, paste.revision_of FROM paste WHERE (paste.id IN (SELECT report.paste FROM report));


--
-- Name: public_paste; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW public_paste AS
    SELECT paste.id, paste.title, paste.content, paste.author, paste.created, paste.views, paste.language, paste.channel, paste.annotation_of, paste.revision_of, paste.spamrating, paste.public FROM paste WHERE (NOT (paste.id IN (SELECT report.paste FROM report)));


--
-- Name: public_toplevel_paste; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW public_toplevel_paste AS
    SELECT public_paste.id, public_paste.title, public_paste.content, public_paste.author, public_paste.created, public_paste.views, public_paste.language, public_paste.channel, public_paste.annotation_of, public_paste.revision_of, public_paste.spamrating FROM public_paste WHERE (((public_paste.annotation_of IS NULL) AND (public_paste.revision_of IS NULL)) AND public_paste.public);


--
-- Name: report_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE report_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: report_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE report_id_seq OWNED BY report.id;


--
-- Name: step; Type: TABLE; Schema: public; Owner: -; Tablespace:
--

CREATE TABLE step (
    paste integer NOT NULL,
    step integer NOT NULL,
    content text NOT NULL
);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY channel ALTER COLUMN id SET DEFAULT nextval('channel_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY hint ALTER COLUMN id SET DEFAULT nextval('hint_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY language ALTER COLUMN id SET DEFAULT nextval('language_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY paste ALTER COLUMN id SET DEFAULT nextval('paste_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY report ALTER COLUMN id SET DEFAULT nextval('report_id_seq'::regclass);


--
-- Name: channel_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace:
--

ALTER TABLE ONLY channel
    ADD CONSTRAINT channel_pkey PRIMARY KEY (id);


--
-- Name: hint_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace:
--

ALTER TABLE ONLY hint
    ADD CONSTRAINT hint_pkey PRIMARY KEY (id);


--
-- Name: language_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace:
--

ALTER TABLE ONLY language
    ADD CONSTRAINT language_pkey PRIMARY KEY (id);


--
-- Name: paste_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace:
--

ALTER TABLE ONLY paste
    ADD CONSTRAINT paste_pkey PRIMARY KEY (id);


--
-- Name: report_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace:
--

ALTER TABLE ONLY report
    ADD CONSTRAINT report_pkey PRIMARY KEY (id);


--
-- Name: paste_author_index; Type: INDEX; Schema: public; Owner: -; Tablespace:
--

CREATE INDEX paste_author_index ON paste USING btree (author);


--
-- Name: paste_date_index; Type: INDEX; Schema: public; Owner: -; Tablespace:
--

CREATE INDEX paste_date_index ON paste USING btree (created);


--
-- Name: paste_title_index; Type: INDEX; Schema: public; Owner: -; Tablespace:
--

CREATE INDEX paste_title_index ON paste USING btree (title);


--
-- Name: hint_paste_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY hint
    ADD CONSTRAINT hint_paste_fkey FOREIGN KEY (paste) REFERENCES paste(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: paste_channel_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY paste
    ADD CONSTRAINT paste_channel_fkey FOREIGN KEY (channel) REFERENCES channel(id);


--
-- Name: paste_language_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY paste
    ADD CONSTRAINT paste_language_fkey FOREIGN KEY (language) REFERENCES language(id);


--
-- Name: paste_revision_of_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY paste
    ADD CONSTRAINT paste_revision_of_fkey FOREIGN KEY (revision_of) REFERENCES paste(id) ON DELETE CASCADE;


--
-- Name: report_paste_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY report
    ADD CONSTRAINT report_paste_fkey FOREIGN KEY (paste) REFERENCES paste(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: step_paste_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY step
    ADD CONSTRAINT step_paste_fkey FOREIGN KEY (paste) REFERENCES paste(id);


--
-- Name: public; Type: ACL; Schema: -; Owner: -
--

REVOKE ALL ON SCHEMA public FROM PUBLIC;
REVOKE ALL ON SCHEMA public FROM postgres;
GRANT ALL ON SCHEMA public TO postgres;
GRANT ALL ON SCHEMA public TO PUBLIC;


--
-- PostgreSQL database dump complete
--

ALTER TABLE paste ADD flaggedspam BOOL NOT NULL DEFAULT FALSE;
