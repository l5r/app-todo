// see https://github.com/mu-semtech/mu-javascript-template for more info

import { app, update, query, sparqlEscapeString, errorHandler } from 'mu';

app.get('/', function(req, res) {
  res.send('Hello mu-javascript-template');
});

app.delete('/todo-lists/:todoListId', async function(req, res) {
  const todoListId = req.params.todoListId;

  const result = await update(`
      PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
      PREFIX todo: <http://mu.semte.ch/vocabularies/ext/>
      DELETE {?s ?p ?o} WHERE {
        {
          SELECT (?todoList as ?s) ?p ?o {
            ?todoList mu:uuid ${sparqlEscapeString(todoListId)}.
            ?todoList ?p ?o
          }
        } UNION {
          SELECT (?todoItem as ?s) ?p ?o {
            ?todoList mu:uuid ${sparqlEscapeString(todoListId)}.
            ?todoList todo:items ?todoItem.
            ?todoItem ?p ?o
          }
        }
      }
    `, {});

    res.send('')
})

app.use(errorHandler);
