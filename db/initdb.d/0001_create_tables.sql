\connect chatbot

CREATE TABLE bot (
    id serial PRIMARY KEY,
    token text NOT NULL,
    topic_id int NOT NULL,
    last_post int NOT NULL,
    created_at timestamp with time zone NOT NULL,
    modified_at timestamp with time zone NOT NULL
);