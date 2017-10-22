CREATE TABLE SnippetMetadata (
    Id            INTEGER         PRIMARY KEY AUTOINCREMENT,
    HighlighterId INTEGER,
    NodeTypeId    INTEGER,
    ParentId      INTEGER,
    ImageIndex    INTEGER,
    NodeName      VARCHAR( 255 ),
    NodePath      TEXT,
    FoldLevel     INTEGER,
    CommentRTF    TEXT,
    FoldState     VARCHAR( 255 ),
    DateCreated   DATETIME,
    DateModified  DATETIME
);

CREATE INDEX idx_SnippetMetadataParentId ON SnippetMetadata (
    ParentId ASC
);

CREATE INDEX idx_SnippetMetadataNodePath ON SnippetMetadata (
    NodePath ASC
);

CREATE INDEX idx_SnippetMetadataNodeName ON SnippetMetadata (
    NodeName DESC
);

CREATE VIRTUAL TABLE SnippetData USING FTS4(
    Text,
    Comment
);

CREATE VIEW Snippet AS
  SELECT
    sm.Id,
    sm.HighlighterId,
    sm.NodeTypeId,
    sm.ParentId,
    sm.ImageIndex,
    sm.NodeName,
    sm.NodePath,
    sm.FoldLevel,
    sm.CommentRTF,
    sm.FoldState,
    sm.DateCreated,
    sm.DateModified,
    sd.Text,
    sd.Comment
  FROM
    SnippetMetadata sm
    INNER JOIN SnippetData sd
      ON (sm.RowId = sd.RowId);

CREATE TRIGGER Snippet_insert INSTEAD OF INSERT ON Snippet
BEGIN
  INSERT INTO SnippetMetadata(
    Id,
    HighlighterId,
    NodeTypeId,
    ParentId,
    ImageIndex,
    NodeName,
    NodePath,
    FoldLevel,
    CommentRTF,
    FoldState,
    DateCreated,
    DateModified
  ) VALUES (
    NEW.Id,
    NEW.HighlighterId,
    NEW.NodeTypeId,
    NEW.ParentId,
    NEW.ImageIndex,
    NEW.NodeName,
    NEW.NodePath,
    NEW.FoldLevel,
    NEW.CommentRTF,
    NEW.FoldState,
    NEW.DateCreated,
    NEW.DateModified
  );
  INSERT INTO SnippetData(
    RowId,
    Text,
    Comment
  ) VALUES (
    last_insert_rowid(),
    NEW.Text,
    NEW.Comment
  );
END;

CREATE TRIGGER Snippet_delete INSTEAD OF DELETE ON Snippet
BEGIN
  DELETE FROM SnippetMetadata WHERE RowId = OLD.RowId;
  DELETE FROM SnippetData WHERE RowId = OLD.RowId;
END;


/*
CREATE TABLE metadata (document INTEGER, page INTEGER, UNIQUE(document, page));
CREATE VIRTUAL TABLE data USING fts4();

CREATE VIEW whole AS SELECT metadata.rowid AS rowid, document, page, content
    FROM metadata JOIN data ON metadata.rowid = data.rowid;

CREATE TRIGGER whole_insert INSTEAD OF INSERT ON whole
BEGIN
  INSERT INTO metadata (document, page) VALUES (NEW.document, NEW.page);
  INSERT INTO data (rowid, content) VALUES (last_insert_rowid(), NEW.content);
END;

CREATE TRIGGER whole_delete INSTEAD OF DELETE ON whole
BEGIN
  DELETE FROM metadata WHERE rowid = OLD.rowid;
  DELETE FROM data WHERE rowid = OLD.rowid;
END;
*/
