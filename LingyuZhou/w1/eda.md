# EDA
## df shape:
1053604 rows × 15 columns

## unique counts:
| columns           | count|
|-------------------|------|
| Country           | 7    |
| FactoryAssessedID | 1978 |
| AssesmentDate     | 1446 |
| year              | 7    |
| month             | 12   |
| QuestID           | 6476 |
| Cluster           | 9    |
| CP                | 36   |
| QLabel            | 788  |
| Finding           | 2    |
| Tags              | 110  |
| disclosed         | 2    |
| Itemscleaned      | 629  |
| CPcleaned         | 35   |
| threestds         | 3    |

## question year:
| country | years      |
|------------|--------------------------------------------|
| Bangladesh | [2015; 2016; 2017; 2018; 2019; 2020]       |
| Cambodia   | [2015; 2016; 2017; 2018; 2019; 2020; 2021] |
| Haiti      | [2015; 2016; 2017; 2018; 2019; 2020; 2021] |
| Indonesia  | [2015; 2016; 2017; 2018; 2019; 2020]       |
| Jordan     | [2015; 2016; 2017; 2018; 2019; 2020; 2021] |
| Nicaragua  | [2015; 2016; 2017; 2018; 2019; 2020]       |
| Vietnam    | [2015; 2016; 2017; 2018; 2019; 2020; 2021] |

# is factory id related unique to country?
```python
df.groupby('FactoryAssessedID').filter(lambda x: x['Country'].nunique() > 1)
```

```shell
> yes
```

How to loop up in the output json:

```python
import json
with open("fid2country.json", 'r') as file:
    js = dict(json.load(file))
[x for x in js if 3479 in js[x]][0]
```