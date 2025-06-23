{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module LS (start) where

import Control.Monad.IO.Class (liftIO)

import qualified Language.LSP.Protocol.Message as Msg
import qualified Language.LSP.Protocol.Types as Types
import qualified Language.LSP.Server
import qualified Language.LSP.Server as Server

capabilities :: Types.ServerCapabilities
capabilities =
  Types.ServerCapabilities
    { _textDocumentSync =
        Just $ Types.InR Types.TextDocumentSyncKind_Full
    , _completionProvider = Nothing
    , _hoverProvider = Just (Types.InL True)
    , _signatureHelpProvider = Nothing
    , _definitionProvider = Nothing
    , _typeDefinitionProvider = Nothing
    , _implementationProvider = Nothing
    , _referencesProvider = Nothing
    , _documentHighlightProvider = Nothing
    , _documentSymbolProvider = Nothing
    , _workspaceSymbolProvider = Nothing
    , _codeActionProvider = Nothing
    , _codeLensProvider = Nothing
    , _documentFormattingProvider = Nothing
    , _documentRangeFormattingProvider = Nothing
    , _documentOnTypeFormattingProvider = Nothing
    , _renameProvider = Nothing
    , _documentLinkProvider = Nothing
    , _colorProvider = Nothing
    , _foldingRangeProvider = Nothing
    , _declarationProvider = Nothing
    , _executeCommandProvider = Nothing
    , _callHierarchyProvider = Nothing
    , _selectionRangeProvider = Nothing
    , _linkedEditingRangeProvider = Nothing
    , _semanticTokensProvider = Nothing
    , _monikerProvider = Nothing
    , _workspace = Nothing
    , _experimental = Nothing
    , _positionEncoding = Just Types.PositionEncodingKind_UTF8
    , _notebookDocumentSync = Nothing
    , _typeHierarchyProvider = Nothing
    , _inlineValueProvider = Nothing
    , _inlayHintProvider = Nothing
    , _diagnosticProvider = Nothing
    -- Just $
    --   Types.InL
    --     Types.DiagnosticOptions
    --       { _workDoneProgress = Nothing
    --       , _identifier = Nothing
    --       , _interFileDependencies = False
    --       , _workspaceDiagnostics = False
    --       }
    }

lspHandlers :: Types.ClientCapabilities -> Language.LSP.Server.Handlers (Language.LSP.Server.LspM ())
lspHandlers _ =
  mconcat
    [ Language.LSP.Server.notificationHandler Msg.SMethod_Initialized $ \_notif -> do
        return ()
    , Language.LSP.Server.requestHandler Msg.SMethod_TextDocumentHover $ \req responder -> do
        let Msg.TRequestMessage _ _ _ (Types.HoverParams _doc pos _workDone) = req
            Types.Position _l _c' = pos
            rsp = Types.Hover (Types.InL ms) (Just range)
            ms = Types.mkMarkdown "Hello world"
            range = Types.Range pos pos
        responder (Right $ Types.InL rsp)
    , Language.LSP.Server.requestHandler Msg.SMethod_Initialize $ \_req responder ->
        responder $
          Right
            Types.InitializeResult
              { Types._capabilities = capabilities
              , Types._serverInfo = Just $ Types.ServerInfo "Example Haskell LSP" (Just "0.1")
              }
    , Language.LSP.Server.notificationHandler Msg.SMethod_WorkspaceDidChangeConfiguration $ \_ -> do
        return ()
    , Language.LSP.Server.notificationHandler Msg.SMethod_TextDocumentDidOpen $ \notif -> do
        Server.sendNotification Msg.SMethod_TextDocumentPublishDiagnostics $
          Types.PublishDiagnosticsParams
            { _uri = notif._params._textDocument._uri
            , _version = Nothing
            , _diagnostics =
                [ Types.Diagnostic
                    { _range =
                        Types.Range
                          { _start =
                              Types.Position
                                { _character = 1
                                , _line = 1
                                }
                          , _end =
                              Types.Position
                                { _character = 3
                                , _line = 1
                                }
                          }
                    , _severity = Just Types.DiagnosticSeverity_Error
                    , _code = Nothing
                    , _codeDescription = Nothing
                    , _source = Nothing
                    , _message = "ERR"
                    , _tags = Nothing
                    , _relatedInformation = Nothing
                    , _data_ = Nothing
                    }
                ]
            }
    , Language.LSP.Server.notificationHandler Msg.SMethod_TextDocumentDidChange $ \notif ->
        Server.sendNotification Msg.SMethod_TextDocumentPublishDiagnostics $
          Types.PublishDiagnosticsParams
            { _uri = notif._params._textDocument._uri
            , _version = Nothing
            , _diagnostics =
                [ Types.Diagnostic
                    { _range =
                        Types.Range
                          { _start =
                              Types.Position
                                { _character = 1
                                , _line = 1
                                }
                          , _end =
                              Types.Position
                                { _character = 10
                                , _line = 1
                                }
                          }
                    , _severity = Just Types.DiagnosticSeverity_Error
                    , _code = Nothing
                    , _codeDescription = Nothing
                    , _source = Nothing
                    , _message = "ERR"
                    , _tags = Nothing
                    , _relatedInformation = Nothing
                    , _data_ = Nothing
                    }
                ]
            }
    ]

start :: IO ()
start = do
  _ <-
    Server.runServer $
      Server.ServerDefinition
        { Server.defaultConfig = ()
        , Server.configSection = ""
        , Server.parseConfig = \_ _ -> Right ()
        , Server.onConfigChange = \_ -> return ()
        , Server.doInitialize = \env _req -> pure $ Right env
        , Server.staticHandlers = lspHandlers
        , Server.interpretHandler = \env ->
            Language.LSP.Server.Iso
              (Language.LSP.Server.runLspT env) -- how to convert from IO ~> m
              liftIO -- how to convert from m ~> IO
        , Server.options = Language.LSP.Server.defaultOptions
        }
  return ()
