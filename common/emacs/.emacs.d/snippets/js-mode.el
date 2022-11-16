(require 'tempo)

(tempo-define-template "js-mode-named-function"
                       '("function " p "(" p ") {" n> r> n> "}" >)
                       "f"
                       "Inserts a define template for function"
                       'javascript-tempo-tags)

(tempo-define-template "js-mode-arrow-function"
                       '("(" p ") => {" n> r> n> "}" >)
                       "af"
                       "Inserts a define template for arrow function"
                       'javascript-tempo-tags)

(tempo-define-template "js-mode-console-log"
                       '("console.log(" r ")")
                       "clg"
                       "Inserts a define template for console log"
                       'javascript-tempo-tags)

(tempo-define-template "js-mode-define-require-js"
                       '("/**" > n
                         "* " > p n
                         "* @author " > p n
                         "* @since " > p n
                         " */" > n
                         "define(['" p "']," > n
                         "function (" p ") {" > n
                         > p n
                         "     return {" > n
                         "       myModule: null" > n
                         "     }" > n
                         "})" >
                         )
                       "define"
                       "Inserts a define template for RequireJS"
                       'javascript-tempo-tags)

(tempo-define-template "js-mode-require-require-js"
                       '("require(['" p "']," > n
                         "function (" p ") {" > n
                         > p n
                         "})" >
                         )
                       "requirejs"
                       "Inserts a require template for RequireJS"
                       'javascript-tempo-tags)

(tempo-define-template "js-mode-import"
                       '("import " p " from '" p "'")
                       "import"
                       "Inserts a import template"
                       'javascript-tempo-tags)

(tempo-define-template "js-mode-log-audit"
                       '("log.audit({title: '" p "', details: '" p "'})")
                       "log.audit"
                       "Inserts a log audit template"
                       'javascript-tempo-tags)

(tempo-define-template "js-mode-module-exports"
                       '("module.exports = " p)
                       "mde"
                       "Inserts a module export template"
                       'javascript-tempo-tags)

(tempo-define-template "js-mode-ssclient"
                       '("/**
* @NApiVersion 2.x
* @NScriptType ClientScript
*/
define([" p "],
  function (" p ") {
"> p "
    return {
      pageInit: null,
      fieldChanged: null,
      postSourcing: null,
      sublistChanged: null,
      lineInit: null,
      validateField: null,
      validateLine: null,
      validateInsert: null,
      validateDelete: null,
      saveRecord: null
    }
  })")
                       "ssclient"
                       "Inserts a module suite script client template"
                       'javascript-tempo-tags)

(tempo-define-template "js-mode-ssmapreduce"
                       '("/**
 *@NApiVersion 2.x
 *@NScriptType MapReduceScript
 */
define([" p "],
  function (" p ") {
"> p "
    return {
      getInputData: null,
      map: null,
      reduce: null,
      summarize: null
    }
  })")
                       "ssmapreduce"
                       "Inserts a module suite script map reduce template"
                       'javascript-tempo-tags)

(tempo-define-template "js-mode-ssmassupdate"
                       '("/**
 *@NApiVersion 2.0
 *@NScriptType MassUpdateScript
 */
define([" p "],
  function (" p ") {
    function each(params) {
" p >"
    }

    return {
      each: each
    }
  })")
                       "ssmassupdate"
                       "Inserts a module suite script mass update template"
                       'javascript-tempo-tags)

(tempo-define-template "js-mode-ssportlet"
                       '("/**
 *@NApiVersion 2.x
 *@NScriptType Portlet
 */
define([" p "],
  function(" p ") {
    function render(params) {
" p > "
    }

    return {
      render: render
    }
  })")
                       "ssportlet"
                       "Inserts a module suite script portlet template"
                       'javascript-tempo-tags)

(tempo-define-template "js-mode-ssrestlet"
                       '("/**
 *@NApiVersion 2.x
 *@NScriptType Restlet
 */
define([" p "],
  function(" p ") {
" p > "

    return {
      get: null,
      delete: null,
      post: null,
      put: null
    }
  })")
                       "ssrestlet"
                       "Inserts a module suite script restlet template"
                       'javascript-tempo-tags)

(tempo-define-template "js-mode-ssschedule"
                       '("/**
 *@NApiVersion 2.x
 *@NScriptType ScheduledScript
 */
define([" p "],
  function(" p ") {
      function execute(context) {
" p > "
      }

      return {
        execute: execute
      }
})")
                       "ssschedule"
                       "Inserts a module suite script schedule template"
                       'javascript-tempo-tags)

(tempo-define-template "js-mode-sssuitelet"
                       '("/**
 *@NApiVersion 2.x
 *@NScriptType Suitelet
 */
define([" p "],
  function(" p ") {
    function onRequest(context) {
      const router = {
        'GET': get,
        'POST': post,
        'PUT': put,
        'DELETE': delete
      }

      const handler = router[context.request.method] || notFound

      return handler(context)
    }

    function get(context) {
    }

    function post(context) {
    }

    function put(context) {
    }

    function delete(context) {
    }

    function notFound(context) {
    }

    return {
      onRequest: onRequest
    }
  })")
                       "sssuitelet"
                       "Inserts a module suite script suitlet template"
                       'javascript-tempo-tags)

(tempo-define-template "js-mode-ssuserevent"
                       '("/**
 *@NApiVersion 2.x
 *@NScriptType UserEventScript
 */
define([" p "],
  function(" p ") {
" p > "
    return {
      beforeLoad: null,
      beforeSubmit: null,
      afterSubmit: null
    }
  })")
                       "ssuserevent"
                       "Inserts a module suite script user event template"
                       'javascript-tempo-tags)
