Tyqu
====
Tyqu provides a convenient Scala API for generating and executing SQL queries.

Supported platforms:
* MySQL
* PostgreSQL


Usage
-----
First we need to define the schema of our database. For simplicity, let's start with an oversimplified book database consisting of two tables: `books` and `authors`.

```scala
import tyqu.*

object BookDatabase:

    object Books extends Table:
        val id = Column[Int](primary = true)
        val title = Column[String]()
        val authorId = Column[Int]()

        lazy val author = ManyToOne(Authors, authorId)


    object Authors extends Table:
        val id = Column[Int](primary = true)
        val firstName = Column[String]()
        val lastName = Column[String]
        val birthYear = Column[Int]()

        lazy val books = OneToMany(Books, Books.author)
```

Now we can generate queries. The starting point of every query is a `from` function call which takes a table object as a parameter.
```scala
import tyqu.*
import BookDatabase.*

val authorsQuery = 
    from(Authors)
        .filter(_.birthYear > 1980)
        .sortBy(_.lastName)
        .limit(10)
```

Tyqu provides a convenient way to refer to related tables without explicitly writing joins. For example, instead of sorting by name, we can easily sort by the number of books that the authors have written.
```scala
val authorsQuery = from(Authors).sortBy(_.books.count)
```

This way of accessing related tables can be used in all query-modifying methods (e.g. `filter`, `map`, `sortBy`, `groupBy`). Thus, we can also easily find all authors who have written something about Scala.
```scala
val authorsQuery = from(Authors).filter(_.books.exists(_.title.contains("Scala")))
```

Similarly we can also access related tables from the other side. For example, here is a query that finds all books whose author's first name is Martin.
```scala
val booksQuery = from(Books).filter(_.author.firstName === "Martin")
```

In order to execute the query, we need to provide a database connection and query executor.
```scala
val connection = DriverManager.getConnection("jdbc:postgresql://localhost:5432/booksdb?user=postgres&password=1234&ssl=false")
given PostgreSqlQueryExecutor(connection)
```

Now we can execute our query, iterate over the results and print them out.
```scala
val results = authorsQuery.execute()
for author <- results do
    println(s"${author.firstName} ${author.lastName} (born ${author.birthYear})")
```

Tyqu provides a convenient way for expressing projections. For example, we can get the full name and age of each author with the following code.
```scala
val currentYear = 2023
val results =
    from(Authors)
        .map{ a => (
            (a.firstName + " " + a.lastName).as("name"),
            (currentYear - a.birthYear).as("age"),
        ) }
        .execute()
for author <- results do
    println(author.name)
    println(author.age)
```

Note that in `map` we are returning a tuple of values but `results` is still an iterator of objects. More specifically, the type of `results` in this case would be
```scala
Iterator[tyqu.execution.Result & {
    val name: String
    val age: Int
}]
```

Tyqu uses macros and type refinements in order to provide the correct result types at compile time and to guarantee type safety.

For convenience, projections to a single value receive special treatment: instead of using the `Result` type, we use directly the value type.
```scala
val results =
    from(Authors)
        .map{ a => a.firstName + " " + a.lastName }
        .execute()
results.foreach(println)
```
Here `results` is of type `Iterator[String]`
