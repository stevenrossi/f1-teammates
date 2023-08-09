SELECT q.driverId,
	   q.constructorId,
       positionText,
       positionOrder,
       q.raceID,
       year,
       date,
       round,
       r.name AS raceName,
       c.name AS constructorName,
       CONCAT(surname, ', ', forename) AS driverName,
       d.number,
       d.url,
       dob,
	   d.nationality
FROM sql_inventory.results q
JOIN races r
	ON q.raceId = r.raceId
JOIN drivers d
	ON q.driverId = d.driverId
JOIN constructors c
	ON q.constructorId = c.constructorId;