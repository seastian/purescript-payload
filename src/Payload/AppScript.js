export const appScriptJson = (content) =>
  ContentService.createTextOutput(content).setMimeType(
    ContentService.MimeType.JSON,
  );
