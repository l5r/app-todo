# frozen_string_literal: true

RESOURCE_VAR = '?__resource'
COUNT_VAR = '?__count'

class Resource
  attr_reader :rdf_class, :name, :attributes, :relationships

  alias jsonapi_name name

  def initialize(rdf_class:, name:, attributes:, relationships:)
    @rdf_class = rdf_class
    @name = name
    @attributes = attributes
    @relationships = relationships
  end

  def base_path
    "/#{jsonapi_name}"
  end

  def variables
    attributes.map(&:variable)
  end

  def query_pattern
    "#{RESOURCE_VAR} a #{rdf_class}.\n" +
      attributes.map(&:query_pattern).join("\n")
  end

  def extract_from_solution(solution)
    attributes.map do |attr|
      [attr.name, attr.extract_from_solution(solution)]
    end.to_h
  end

  def to_jsonapi(resources)
    {
      data: resources.map { |r| to_jsonapi_data(r) },
      meta: { count: resources.length }
    }
  end

  def to_jsonapi_data(resource_hash)
    id = resource_hash.delete(:id)
    {
      id: id,
      type: jsonapi_name,
      attributes: resource_hash,
      relationships: to_jsonapi_relationships(id)
    }
  end

  def to_jsonapi_relationships(id)
    base_path = self.base_path
    relationships.map do |relationship|
      [relationship.name, relationship.to_jsonapi(base_path, id)]
    end.to_h
  end
end

class Attribute
  attr_reader :name, :rdf_predicate

  def initialize(name, rdf_predicate)
    @name = name
    @rdf_predicate = rdf_predicate
  end

  def query_pattern
    "#{RESOURCE_VAR} #{rdf_predicate} #{variable} ."
  end

  def variable
    "?#{name}"
  end

  def extract_from_solution(solution)
    transformer(solution[name])
  end

  def transformer(val) = val
end

class CountAttribute < Attribute
  def initialize(name, count_query_pattern)
    super(name, nil)
    @count_query_pattern = count_query_pattern
  end

  def query_pattern
    <<-SPARQL
    OPTIONAL {
      SELECT #{RESOURCE_VAR} (COUNT(#{COUNT_VAR}) AS #{variable}) {
        #{@count_query_pattern}
      } GROUP BY #{RESOURCE_VAR}
    }
    SPARQL
  end

  def transformer(val) = (val or 0).to_i
end

class Relationship
  attr_reader :name

  def initialize(name)
    @name = name
  end

  def to_jsonapi(base_path, id)
    {
      links: {
        self: "#{base_path}/#{id}/links/#{name}",
        related: "#{base_path}/#{id}/#{name}"
      }
    }
  end
end
