/*********************************************************************************
# Copyright 2015 Observational Health Data Sciences and Informatics
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
#     http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
********************************************************************************/
{DEFAULT @study_start_date = '19000101'} 
{DEFAULT @study_end_date = '21000101'} 
{DEFAULT @cdm_database_schema = 'dcm4'} 
{DEFAULT @split_by_age_group = TRUE} 
{DEFAULT @split_by_year = TRUE} 
{DEFAULT @split_by_gender = TRUE}

SELECT SUM(1 + CAST(DATEDIFF(DAY, start_date, end_date) AS BIGINT)) AS days,
	COUNT_BIG(DISTINCT person_id) AS persons 
{@split_by_age_group} ? {	,age_group} 
{@split_by_year} ? {	,calendar_year}
{@split_by_gender} ? {	,gender_concept_id}
FROM (
	SELECT person_id,
		age_group,
		calendar_year,
		gender_concept_id,
		CASE 
			WHEN start_date < calendar_year_start_date
				THEN calendar_year_start_date
			ELSE start_date
			END AS start_date,
		CASE 
			WHEN end_date > calendar_year_end_date
				THEN calendar_year_end_date
			ELSE end_date
			END AS end_date
	FROM (
		SELECT temp1.person_id,
			age_group.age_group AS age_group,
			gender_concept_id,
			CASE 
				WHEN start_date < DATEADD(DAY, CAST(start_age * 365.25 AS INT), date_of_birth)
					THEN DATEADD(DAY, CAST(start_age * 365.25 AS INT), date_of_birth)
				ELSE start_date
				END AS start_date,
			CASE 
				WHEN end_date > DATEADD(DAY, CAST(end_age * 365.25 AS INT) - 1, date_of_birth)
					THEN DATEADD(DAY, CAST(end_age * 365.25 AS INT) - 1, date_of_birth)
				ELSE end_date
				END AS end_date
		FROM (
			SELECT person.person_id,
				DATEFROMPARTS(year_of_birth, ISNULL(month_of_birth, 7), ISNULL(day_of_birth, 1)) AS date_of_birth,
				gender_concept_id,
				CASE 
					WHEN '@study_start_date' > observation_period_start_date
						THEN '@study_start_date'
					ELSE observation_period_start_date
					END AS start_date,
				CASE 
					WHEN '@study_end_date' < observation_period_end_date
						THEN '@study_end_date'
					ELSE observation_period_end_date
					END AS end_date
			FROM @cdm_database_schema.person
			INNER JOIN @cdm_database_schema.observation_period
				ON person.person_id = observation_period.person_id
			WHERE '@study_start_date' <= observation_period_end_date
			    AND '@study_end_date' >= observation_period_start_date
			) temp1
		INNER JOIN #study_population study_population
		    ON temp1.person_id = study_population.person_id
		INNER JOIN #age_group age_group
			ON start_date < DATEADD(DAY, CAST(end_age * 365.25 AS INT), date_of_birth)
				AND end_date >= DATEADD(DAY, CAST(start_age * 365.25 AS INT), date_of_birth)
		) temp2
	INNER JOIN #year_period calendar_year
		ON start_date <= calendar_year_end_date
			AND end_date >= calendar_year_start_date
	) temp3 
{@split_by_age_group | @split_by_year | @split_by_gender} ? {
GROUP BY 
{@split_by_age_group} ? {	age_group} 
{@split_by_year} ? {{@split_by_age_group} ? {	,} calendar_year} 
{@split_by_gender} ? {{@split_by_age_group | @split_by_year} ? {	,} gender_concept_id} 
};
