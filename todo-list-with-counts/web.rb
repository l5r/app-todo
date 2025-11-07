# frozen_string_literal: true

# see https://github.com/mu-semtech/mu-ruby-template for more info

require_relative './resource'

get '/todo-lists/with-counts' do
  content_type 'application/json'

  result = Mu.query(<<-SPARQL)
    PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
    PREFIX todo: <http://mu.semte.ch/vocabularies/ext/>
    SELECT #{TODO_LIST_RESOURCE.variables.join(' ')} {
      #{TODO_LIST_RESOURCE.query_pattern}
    }
  SPARQL

  data = result.map do |solution|
    TODO_LIST_RESOURCE.extract_from_solution(solution)
  end

  TODO_LIST_RESOURCE.to_jsonapi(data).to_json
end

TODO_LIST_RESOURCE = Resource.new(
  rdf_class: 'todo:TodoList',
  name: 'todo-lists',
  attributes: [
    Attribute.new(:id, 'mu:uuid'),
    Attribute.new(:title, 'todo:title'),
    CountAttribute.new(:total, <<-SPARQL),
      #{RESOURCE_VAR} todo:items #{COUNT_VAR} .
    SPARQL
    CountAttribute.new(:completed, <<-SPARQL)
      #{RESOURCE_VAR} todo:items #{COUNT_VAR} .
      #{COUNT_VAR} todo:completedAt ?c .
    SPARQL
  ],
  relationships: [
    Relationship.new(:items)
  ]
)
