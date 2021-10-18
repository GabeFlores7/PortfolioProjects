Select *
From PortfolioProject..CovidDeaths
where continent is not null
order by 3,4

--Select *
--From PortfolioProject..CovidVaccinations
--order by 3,4

--Select Data that we are going to be using

Select Location, date, total_cases, new_cases, total_deaths, population
From PortfolioProject..CovidDeaths
where continent is not null
order by 1,2


--Looking at Total Cases vs Total Deaths (as %)
--shows likelihood of dying if you contract covid in your country
Select Location, date, total_cases, total_deaths, round((total_deaths/total_cases)*100, 2) as DeathPercentage
From PortfolioProject..CovidDeaths
where location like '%states%'
and continent is not null
order by 1,2



---Looking at Total Cases vs Population
--percentage of population that was infected with covid
Select Location, date, population, total_cases, round((total_cases/population) *100, 2) as percentContracted
From PortfolioProject..CovidDeaths
where location like '%states%'
and continent is not null
order by 1,2


--Looking at countries with highest infection rate compared to population
Select Location, population, max(total_cases) as
madInfectionCount, round((max(total_cases/population))*100,3) as percPopulationInfected
From PortfolioProject..CovidDeaths
Where continent is not null
Group by Location, population
order by percPopulationInfected desc




--Showing countries with highest death count per population
Select Location, max(cast(total_deaths as int)) as totalDeathCount
From PortfolioProject..CovidDeaths
Where continent is not null
Group by Location
order by totalDeathCount desc


--group and analyze by continent

Select location, max(cast(total_deaths as int)) as totalDeathCount
From PortfolioProject..CovidDeaths
Where continent is null
Group by location
order by totalDeathCount desc

Select continent, max(cast(total_deaths as int)) as totalDeathCount
From PortfolioProject..CovidDeaths
Where continent is not null
Group by continent
order by totalDeathCount desc



--Global Numbers
--Including time

Select date, sum(new_cases) as totalNewCases, sum(cast(new_deaths as int)) as
totalNewDeaths, round(sum(cast(new_deaths as int))/sum(new_cases) *100, 2) as
NewDeathPerc--, sum(total_deaths)
From PortfolioProject..CovidDeaths
--where location like '%states%'
Where continent is not null
Group by date
order by 1,2


--Total aggregate numbers
Select sum(new_cases) as totalNewCases, sum(cast(new_deaths as int)) as
totalNewDeaths, round(sum(cast(new_deaths as int))/sum(new_cases) *100, 2) as
NewDeathPerc--, sum(total_deaths)
From PortfolioProject..CovidDeaths
--where location like '%states%'
Where continent is not null
--Group by date
order by 1,2


--Looking at Total Population vs Vaccinations

Select dea.continent, dea.location, dea.date, dea.population, vac.new_vaccinations
, sum(cast(vac.new_vaccinations as bigint)) OVER (Partition by dea.location Order by
dea.location, dea.date) as rollingPersonsVaccinated
From PortfolioProject..CovidDeaths as dea
Join PortfolioProject..CovidVaccinations as vac
	on dea.location = vac.location
	and dea.date = vac.date
Where dea.continent is not null
Order By 2,3


--use CTE

with PopVsVac(Continent, Location, Date, Population, new_vaccinations,
RollingPersonsVaccinated)
as
(
Select dea.continent, dea.location, dea.date, dea.population, vac.new_vaccinations
, sum(cast(vac.new_vaccinations as bigint)) OVER (Partition by dea.location Order by
dea.location, dea.date) as rollingPersonsVaccinated
From PortfolioProject..CovidDeaths as dea
Join PortfolioProject..CovidVaccinations as vac
	on dea.location = vac.location
	and dea.date = vac.date
Where dea.continent is not null
--order by 2,3
)
Select *, (RollingPersonsVaccinated/Population)*100 as RollingPercVaccinated
From PopVsVac


--Temp Table
Drop Table if exists #PercentPopulationVaccinated
create Table #PercentPopulationVaccinated
(
continent nvarchar(255),
location nvarchar(255),
date datetime,
population numeric,
new_vaccinations numeric,
rollingPersonsVaccinated numeric
)

insert into #PercentPopulationVaccinated
Select dea.continent, dea.location, dea.date, dea.population, vac.new_vaccinations
, sum(cast(vac.new_vaccinations as bigint)) OVER (Partition by dea.location Order by
dea.location, dea.date) as rollingPersonsVaccinated
From PortfolioProject..CovidDeaths as dea
Join PortfolioProject..CovidVaccinations as vac
	on dea.location = vac.location
	and dea.date = vac.date
Where dea.continent is not null
Order By 2,3

select *, (rollingPersonsVaccinated/population)*100
From #PercentPopulationVaccinated

-- Creating View to store data for later visualizations

Create View PercentPopulationVaccinated2 as
Select dea.continent, dea.location, dea.date, dea.population, vac.new_vaccinations
, sum(cast(vac.new_vaccinations as bigint)) OVER (Partition by dea.location Order by
dea.location, dea.date) as rollingPersonsVaccinated
From PortfolioProject..CovidDeaths as dea
Join PortfolioProject..CovidVaccinations as vac
	on dea.location = vac.location
	and dea.date = vac.date
Where dea.continent is not null

Select *
From PercentPopulationVaccinated2
