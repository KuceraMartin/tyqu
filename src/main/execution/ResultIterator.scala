package tyqu.execution

import java.sql.*


class ResultIterator[R](resultSet: ResultSet, conversion: ResultSet => R) extends Iterator[R]:

	private var hasNextRes: Option[Boolean] = None

	def hasNext: Boolean =
		hasNextRes match
			case Some(res) => res
			case None =>
				hasNextRes = Some(resultSet.next())
				hasNextRes.get
		

	def next(): R = 
		hasNextRes = None
		conversion(resultSet)
