-- Table: CommentType
CREATE TABLE CommentType ( 
    ID         INTEGER PRIMARY KEY AUTOINCREMENT
                       UNIQUE,
    Name       TEXT,
    StartTag   TEXT,
    EndTag     TEXT,
    SingleLine BOOLEAN DEFAULT ( 0 ) 
);


-- Table: Glyph
CREATE TABLE Glyph ( 
    ID         INTEGER PRIMARY KEY AUTOINCREMENT,
    Image      BLOB,
    Name       TEXT,
    ImageIndex INTEGER UNIQUE 
);


-- Table: Highlighter
CREATE TABLE Highlighter ( 
    ID                      INTEGER PRIMARY KEY AUTOINCREMENT,
    StreamCommentTypeID     INTEGER,
    SingleLineCommentTypeID INTEGER,
    Code                    TEXT    UNIQUE,
    Name                    TEXT,
    Description             TEXT,
    ImageIndex              INTEGER 
);


-- Table: Snippet
CREATE TABLE Snippet ( 
    ID            INTEGER         PRIMARY KEY AUTOINCREMENT,
    HighlighterID INTEGER,
    NodeTypeID    INTEGER,
    ParentID      INTEGER,
    ImageIndex    INTEGER,
    NodeName      VARCHAR( 255 ),
    NodePath      TEXT,
    Text          TEXT,
    FoldLevel     INTEGER,
    Comment       TEXT,
    CommentRTF    TEXT,
    FoldState     VARCHAR( 255 ),
    DateCreated   DATETIME,
    DateModified  DATETIME 
);


-- Table: NodeType
CREATE TABLE NodeType ( 
    ID         INTEGER PRIMARY KEY AUTOINCREMENT
                       UNIQUE,
    Name       TEXT,
    ImageIndex INTEGER 
);


-- Index: idx_SnippetParentID
CREATE INDEX idx_SnippetParentID ON Snippet ( 
    ParentID ASC 
);
;

-- Index: idx_SnippetNodeName
CREATE INDEX idx_SnippetNodeName ON Snippet ( 
    NodeName DESC 
);
;

-- Index: idx_SnippetNodePath
CREATE INDEX idx_SnippetNodePath ON Snippet ( 
    NodePath ASC 
);
;

-- Index: idx_HighlighterCode
CREATE UNIQUE INDEX idx_HighlighterCode ON Highlighter ( 
    Code 
);
;


