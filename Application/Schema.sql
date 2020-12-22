-- Your database schema. Use the Schema Designer at http://localhost:8001/ to add some tables.
CREATE TABLE books (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    title TEXT NOT NULL,
    page_count INT NOT NULL,
    review TEXT DEFAULT NULL,
    has_read BOOLEAN DEFAULT false NOT NULL,
    published_at TIMESTAMP WITH TIME ZONE NOT NULL
);