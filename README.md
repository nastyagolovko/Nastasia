# [Метрические алгоритмы](https://github.com/nastyagolovko/Nastasia/blob/master/README.md#%D0%BC%D0%B5%D1%82%D1%80%D0%B8%D1%87%D0%B5%D1%81%D0%BA%D0%B8%D0%B5-%D0%B0%D0%BB%D0%B3%D0%BE%D1%80%D0%B8%D1%82%D0%BC%D1%8B)

[Алгоритм 1NN](https://github.com/nastyagolovko/Nastasia/blob/master/README.md#%D0%B0%D0%BB%D0%B3%D0%BE%D1%80%D0%B8%D1%82%D0%BC-1nn)

[Алгоритм kNN](https://github.com/nastyagolovko/Nastasia/blob/master/README.md#%D0%B0%D0%BB%D0%B3%D0%BE%D1%80%D0%B8%D1%82%D0%BC-knn)

[Метод парзеновского окна](https://github.com/nastyagolovko/Nastasia/blob/master/README.md#%D0%BC%D0%B5%D1%82%D0%BE%D0%B4-%D0%BF%D0%B0%D1%80%D0%B7%D0%B5%D0%BD%D0%BE%D0%B2%D1%81%D0%BA%D0%BE%D0%B3%D0%BE-%D0%BE%D0%BA%D0%BD%D0%B0)

[Алгоритм СТОЛП (STOLP)](https://github.com/nastyagolovko/Nastasia/blob/master/README.md#%D0%B0%D0%BB%D0%B3%D0%BE%D1%80%D0%B8%D1%82%D0%BC-%D1%81%D1%82%D0%BE%D0%BB%D0%BF-stolp)

[Метод потенциальных функций](https://github.com/nastyagolovko/Nastasia/blob/master/README.md#%D0%BC%D0%B5%D1%82%D0%BE%D0%B4-%D0%BF%D0%BE%D1%82%D0%B5%D0%BD%D1%86%D0%B8%D0%B0%D0%BB%D1%8C%D0%BD%D1%8B%D1%85-%D1%84%D1%83%D0%BD%D0%BA%D1%86%D0%B8%D0%B9)

[Сравнение алгоритмов классификации](https://github.com/nastyagolovko/Nastasia#%D1%81%D1%80%D0%B0%D0%B2%D0%BD%D0%B5%D0%BD%D0%B8%D0%B5-%D0%B0%D0%BB%D0%B3%D0%BE%D1%80%D0%B8%D1%82%D0%BC%D0%BE%D0%B2-%D0%BA%D0%BB%D0%B0%D1%81%D1%81%D0%B8%D1%84%D0%B8%D0%BA%D0%B0%D1%86%D0%B8%D0%B8)

# [Байесовские методы классификации](https://github.com/nastyagolovko/Nastasia#%D0%B1%D0%B0%D0%B9%D0%B5%D1%81%D0%BE%D0%B2%D1%81%D0%BA%D0%B8%D0%B5-%D0%BC%D0%B5%D1%82%D0%BE%D0%B4%D1%8B-%D0%BA%D0%BB%D0%B0%D1%81%D1%81%D0%B8%D1%84%D0%B8%D0%BA%D0%B0%D1%86%D0%B8%D0%B8)

[Линии уровня](https://github.com/nastyagolovko/Nastasia#%D0%BB%D0%B8%D0%BD%D0%B8%D0%B8-%D1%83%D1%80%D0%BE%D0%B2%D0%BD%D1%8F)

[Подстановочный алгоритм](https://github.com/nastyagolovko/Nastasia#%D0%BF%D0%BE%D0%B4%D1%81%D1%82%D0%B0%D0%BD%D0%BE%D0%B2%D0%BE%D1%87%D0%BD%D1%8B%D0%B9-%D0%B0%D0%BB%D0%B3%D0%BE%D1%80%D0%B8%D1%82)

[Линейный дискриминант Фишера](https://github.com/nastyagolovko/Nastasia#%D0%BB%D0%B8%D0%BD%D0%B5%D0%B9%D0%BD%D1%8B%D0%B9-%D0%B4%D0%B8%D1%81%D0%BA%D1%80%D0%B8%D0%BC%D0%B8%D0%BD%D0%B0%D0%BD%D1%82-%D1%84%D0%B8%D1%88%D0%B5%D1%80%D0%B0)

# [Линейные алгоритмы классификации](https://github.com/nastyagolovko/Nastasia#линейные-классиикаторы)

[Метод стахостического градиента](https://github.com/nastyagolovko/Nastasia#метод-стахостического-градиента)





# Метрические алгоритмы
# Алгоритм 1NN
Подбирается метрика. В данной работе это декартово расстояние между векторами.
Обучающая выборка сортируется в порядке увеличения расстояния от классифицируемого элемента.
Элемент относят к тому классу к которому принадлежит ближайший (первый в отсортированной выборке) элемент.

<p><img src= "1NN.jpg">
  
## Преимущества:

* простота реализации;

* интерпретируемость решений;

## Недостатки:

* низкое качество классификации;

* приходится хранить всю выборку целиком.

# Алгоритм kNN
Для классификации каждого из объектов тестовой выборки необходимо последовательно выполнить следующие операции:

* Вычислить расстояние до каждого из объектов обучающей выборки

* Отобрать k объектов обучающей выборки, расстояние до которых минимально

* Класс классифицируемого объекта — это класс, наиболее часто встречающийся среди k ближайших соседей
kNN — один из простейших алгоритмов классификации, поэтому на реальных задачах он зачастую оказывается неэффективным. Помимо точности классификации, проблемой этого классификатора является скорость классификации: если в обучающей выборке N объектов, в тестовой выборе M объектов, а размерность пространства — K, то количество операций для классификации тестовой выборки может быть оценено как O(K*M*N). И тем не менее, алгоритм работы kNN является хорошим примером для начала знакомства с Machine Learning.


   
  ``` R
  Задаем евклидовое расстояние
  uclideanDistance <- function(u, v)
   {
   sqrt(sum(u - v)^2)
   }
   ``` 
   
``` R
 Применяем метод kNN 
 kNN <- function(xl, z, k) {    
 
Сортируем выборку согласно классифицируемого объекта     
 orderedXl <- sortObjectsByDist(xl, z)     
 n <- dim(orderedXl)[2] - 1         
 
  Получаем классы первых k соседей    
 classes <- orderedXl[1:k, n + 1] 
```


<p><img src= "photofacefun_com_1539847206.jpg">

Преимущества:

* Программная реализация алгоритма относительно проста.

* Результат работы алгоритма легко поддаётся интерпретации. 

* Возможность модификации алгоритма, путём использования наиболее подходящих функций сочетания и метрик позволяет подстроить алгоритм под конкретную задачу.

Проблемы:

* Модель нельзя "отделить" от данных: для классификации нового примера нужно использовать все примеры. Эта особенность сильно ограничивает использование алгоритма


<p><img src= "loo.png">
  <p><img src= "kwnn.png">

# Сравнение качества алгоритмов kNN и kwNN.
kNN — один из простейших алгоритмов классификации, поэтому на реальных задачах он зачастую оказывается неэффективным. Основными проблемами классификатора  является точность и скорость классификации: если в обучающей выборке N объектов, в тестовой выборе M объектов, а размерность пространства — K, то количество операций для классификации тестовой выборки может быть оценено как O(KMN).
кации.

```R
Применение метоа kNN и сортировка выборки согласно классифицируемого объекта и ррасстояния
  KWNN <- function(xl, z, k, q) { 
  orderedXl <- sortObjectsByDist(xl, z, euclideanDistance)
 
 
  Метод скользящего контроля для подбора оптимального k
   LOOKWNN <- function(classificator){                    
   j <- 1
   LOO <- seq(1, length(segment))
  
  
  Берем размерность по толбцам и получаем класс первых k соседей
    n <- dim(orderedXl)[2] - 1 
    classes <- orderedXl[1:k, n + 1]
  
  
  Реализация весовой функции
    w <- q ^ i                                             
    m[[classes[i]]] <- m[[classes[i]]] + w                  
  }
```
kwNN отличается от kNN, тем что учитывает порядок соседей классифицируемого объекта, улчшая качество классификации.

# Метод парзеновского окна 
В данном методе мы используем весовую фнкцию как функцию расстояния. Для реаалиации мы используем параметр h, который называем шириной окна. Данный параметр исползуем прмерно так же как и число соседей k.

Програмный код:
```R
parzen <- function(xl, h, distances, type_core) {
 *Оценка весовой функции по расстоянию, а не по рангу; 
 *h- ширина окна; 
 *distances-расстояние от точки z до каждого объекта выборки xl;
 *type_core- текущая функция ядра


Классы объектов выборки и таблица весов классов
  classes <- xl[1:l, n] 
  weights <- table(classes) 
  weights[1:length(weights)] <- 0
  
  
  Прибавляем его вес к общему весу его класса
    r <- distances[i] / h
    weights[class] <- weights[class] + type_core(r) 
  }


Если веса точки по классам не равны 0, то точка попала в окно; возвращаем класс с максимальным весом 
   if (max(weights) != 0) 
      return (names(which.max(weights))) 
   else
      return (0) 
```

<p><img src= "E.png">
  
  core.E <- function(r) (3/4)*(1-r^2)*(abs(r) <= 1) Ядро Епанечникова
  
<p><img src= "Q.png">
  
  core.Q <- function(r) (15/16)*((1 - r^2)^2)*(abs(r) <= 1) Квадратичесское ядро
  
<p><img src= "T.png">
  
  core.T <- function(r) (1 - abs(r))*(abs(r) <= 1) Треугольное ядро
  
<p><img src= "G.png">
  
  core.G <- function(r) (2*pi)^(-0.5)*exp(-0.5*(r^2)) Гауссовское ядро
  
  
<p><img src= "парзокно.png">
  
  core.P <- function(r) (0.5)*(abs(r) <= 1) Прямоугольное ядро
  

<p><img src= "графики ядер.png">


```R
core.E <- function(r) (3/4)*(1-r^2)*(abs(r) <= 1) Ядро Епанечникова
core.Q <- function(r) (15/16)*((1 - r^2)^2)*(abs(r) <= 1) Квадратичесское ядро
core.T <- function(r) (1 - abs(r))*(abs(r) <= 1) Треугольное ядро
core.G <- function(r) (2*pi)^(-0.5)*exp(-0.5*(r^2)) Гауссовское ядро
core.P <- function(r) (0.5)*(abs(r) <= 1) Прямоугольное ядро
```



# Алгоритм СТОЛП (STOLP)
Алгоритм СТОЛП (STOLP) — алгоритм отбора эталонных объектов для метрического классификатора. 
Результатом работы алгоритма являетя разбиене обучающих объектов на 3 категории: шумовые, эталонные и неинформативные Если гипотеза компактности верна. то выборка достаточно велика, то основная масса обучающх объктов окажется наинфоративной и будет отброшена. Фактически, этот алгоритм выполняет сжатие исодных даных.
<p><img src= "объекты.png">

# Метод потенциальных функций  
  Метрический классификатор, частный случай метода ближайших соседей. Позволяет с помощью простого алгоритма оценивать вес объектов обучающей выборки при решении задачи классификации. 

  В процессе обучения с каждой точкой пространства изображений, соответствующей единичному объекту из обучающей последовательности, связывается функция U(X, Xi), заданная на всем пространстве и зависящая от Xi как от параметра. Такие функции называются потенциальными, так как они напоминают функции потенциала электрического поля вокруг точечного электрического заряда. Изменение потенциала электрического поля по мере удаления от заряда обратно пропорционально квадрату расстояния. Потенциал, таким образом, может служить мерой удаления точки от заряда. Когда поле образовано несколькими зарядами, потенциал в каждой точке этого поля равен сумме потенциалов, создаваемых в этой точке каждым из зарядов. Если заряды, образующие поле, расположены компактной группой, потенциал поля будет иметь наибольшее значение внутри группы зарядов и убывать по мере удаления от нее.

```R
Задаем ширину окна для каждого объекта выборки:
getHWidth <- function(xl) {
  l <- nrow(xl)
  h <- rep(0, l)
  for(i in 1:l) { 
    h[i] <- 0.4 ширину окна задаём как 0.4, так это значение является оптимальным
  }
  return (h)
}



Считаем количество ошибок на выборке
distances_to_points <- matrix(0, l, l)
  err <- eps + 1 будем считать количество ошибок на выборке
  
  
  
  
  
  Считаем количество ошибок:
    err <- 0
    for (i in 1:l) {
      class <- potentialFunction(distances_to_points[i, ], potentials, h, xl, type_core)
      err <- err + (class != xl[i, n])
    }
```

core.E <- function(r) (3/4)(1-r^2)(abs(r) <= 1) Ядро Епанечникова
<p><img src= "Е ядро.PNG">
  
core.Q <- function(r) (15/16)((1 - r^2)^2)(abs(r) <= 1) Квадратичесское ядро
<p><img src= "К ядро.PNG">

* Достоинства метода потенциальных функций заключаются в нелинейном разбиении множества объектов. Что позволяет решать задачи, которые сложно решить другими методами.
* Недостатки — в трудном выборе подходящей потенциальной функции и трудоемкости вычислений, при большом объеме обучающей выборке.


## Сравнение алгоритмов классификации

<table>
<tr>
<td>Метод 1</td>
<td> 1NN </td>
<td> KNN </td>
<td> KWNN </td>
<td> Парзеновское окно </td>
</tr>
<tr>
<td> Параметры</td>
<td>  </td>
<td> К=6 </td>
<td> K=9 </td>
<td> h=0,4; h=0,1 (Гауссовское) </td>
</tr>
<tr>
<td> Величина ошибок</td>
<td> 0,47 </td>
<td> 0,33 </td>
<td> 0,33 </td>
<td> 0,4 </td>
</tr>
</table>


# Байесовские методы классификации
  Если плотность распределения классов известны, то алгоритм классификации,имеющий миимальную вероятность ошибок,можно выплнить в явном виде.
* Априаорная вероятность-вероятность появления объектов каждого из класса. 
* Условная верятность P(y(x)) называется апостериорной вероятностью класса у для объекта х. 

# Линии уровня

  Вероятностное распределение с плотностью называется n-мерным нормальным(Гауссовским) распределенем с мат.ожиданием (центром)  и ковариационной матрицей.
  Если признаки некоррелированы, то линии уровня плотности имеют форму эллипслидов с центром M и осями, параллельными осям координат.
<p><img src= "линиии уровня.png">

<p><img src= "лини уровня2.png">
  
<p><img src= "линии уровня3.png">
  
  Если признаки имеют одинаковые дисперсии, то эллипсоиды являются сферами.
  
  Если признаки коррелированы, то матрица не диагоняльна и линии уровня имеют форму эллипсоидов, оси которых повернуты(направлены вдоль собственных векторов матрицы) относительно исходной системы координат. 
  
  
  # Подстановочный алгорит
  
  Нормальный дискриминантный анализ — это один из вариантов байесовской классификации, в котором в качестве моделей восстанавливаемых плотностей рассматривают многомерные нормальные плотности:  <p><img src= "формула 1.PNG">  где <p><img src= "формула 2.PNG"> — математическое ожидание (центр),<p><img src= "формула 3.PNG"> — ковариационная матрица. Предполагается, что матрица 𝛴 симметричная, невырожденная, положительно определённая. Восстанавливая параметры нормального распределения 𝜇O,𝛴O для каждого класса 𝑦∈𝑌 и подставляя в формулу оптимального байесовского классификатора восстановленные плотности, получим подстановочный (plug-in) алгоритм классификации либо линейный дискриминант Фишера (если предположить, что матрицы ковариации равны для всех классов). Параметры нормального распределения оценивают согласно принципа максимума правдоподобия: 
  <p><img src= "формула 4.PNG">
  
 Напишем программу, которая строит подстановочный (plug-in) алгоритм.  
  
  ```R
  ## Восстановление центра нормального распределения 
estimateMu <- function(objects) 
{     


## mu = 1 / m  *  sum_{i=1}^m(objects_i)     
rows <- dim(objects)[1]     
cols <- dim(objects)[2]          
mu <- matrix(NA, 1, cols)     
for (col in 1:cols)      
{         
mu[1, col] = mean(objects[,col])     
}          
return(mu) 
} 


 ##  Восстановление ковариационной матрицы нормального распределения 
estimateCovarianceMatrix <- function(objects, mu) 
{     
rows <- dim(objects)[1]     
cols <- dim(objects)[2]     
sigma <- matrix(0, cols, cols)          
for (i in 1:rows)     
{         
sigma <- sigma + (t(objects[i,] - mu) %*% 
(objects[i,] - mu)) / (rows - 1)     
}          
return (sigma) 
} 


 ## Получение коэффициентов подстановочного алгоритма 
getPlugInDiskriminantCoeffs <- function(mu1, sigma1, mu2, sigma2) 
{     


## Line equation: a*x1^2 + b*x1*x2 + c*x2 + d*x1 + e*x2 + f = 0     
invSigma1 <- solve(sigma1) 
    invSigma2 <- solve(sigma2)          
f <- log(abs(det(sigma1))) - log(abs(det(sigma2))) + mu1 %*% invSigma1 %*% t(mu1) - mu2 %*% invSigma2 %*% t(mu2);          
alpha <- invSigma1 - invSigma2     
a <- alpha[1, 1]     
b <- 2 * alpha[1, 2]    
c <- alpha[2, 2]          
beta <- invSigma1 %*% t(mu1) - invSigma2 %*% t(mu2)     
d <- -2 * beta[1, 1]     
e <- -2 * beta[2, 1] 
     return (c("x^2" = a, "xy" = b, "y^2" = c, "x" = d, "y" = e, "1" = f)) 
} 


## Оценивание 
objectsOfFirstClass <- xl[xl[,3] == 1, 1:2] 
objectsOfSecondClass <- xl[xl[,3] == 2, 1:2] 
mu1 <- estimateMu(objectsOfFirstClass) 
mu2 <- estimateMu(objectsOfSecondClass) 
 sigma1 <- estimateCovarianceMatrix(objectsOfFirstClass, mu1) 
 sigma2 <- estimateCovarianceMatrix(objectsOfSecondClass, mu2) 
 coeffs <- getPlugInDiskriminantCoeffs(mu1, sigma1, mu2, sigma2) 
 
 
 ## Рисуем дискриминантую функцию – красная линия 
x <- y <- seq(-10, 20, len=100)  
z <- outer(x, y, function(x, y) coeffs["x^2"]*x^2 +  coeffs["xy"]*x*y + coeffs["y^2"]*y^2 + coeffs["x"]*x + coeffs["y"]*y + coeffs["1"])  
 contour(x, y, z, levels=0, drawlabels=FALSE, lwd = 3, col = "red", add = TRUE) 
 
  ```
  <p><img src= "подстан.png">
  
  Недостатки подстановочного алгоритма вытекают из нескольких чрезмерно сильных базовых предположений, которые на практике часто не выполняются.
  * Функции правдоподобия классов могут существенно отличаться от гауссовских. В частности, когда имеются признаки, принимающие дискретные значения, или когда классы распадаются на изолированные сгустки.
  * Если длина выборки меньше размерности пространства, или среди признаков есть линейно зависимые, то матрица становится вырожденной. В этом случае обратная матрица не существует и метод вообще не применим.


# Линейный дискриминант Фишера
   Линейный дискриминант Фишера (ЛДФ), который, в отличии от подстановочного алгоритма, при построении предполагает, что ковариационные матрицы классов равны, и для их восстановления нужно использовать все (всех классов) объекты обучающей выборки. 

   Простота классификации линейным дискриминантом Фишера - одно из достоинств алгоритма: в случае с двумя классами в двумерном признаковом пространстве разделяющей поверхностью будет прямая. Если классов больше двух, то разделяющая поверхность будет кусочно-лиинейной. Но главным преимуществом алгоритма по сравнению с квадратичным дискриминантом является уменьшение эффекта плохой обусловленности ковариационной матрицы при недостаточных данных. 
 
```R
## Оценка ковариационной матрицы для ЛДФ 
estimateFisherCovarianceMatrix <- function(objects1, objects2, mu1, mu2)  
{     
rows1 <- dim(objects1)[1]     
rows2 <- dim(objects2)[1]     
rows <- rows1 + rows2     
cols <- dim(objects1)[2]          
sigma <- matrix(0, cols, cols)          
for (i in 1:rows1)     
{         
sigma <- sigma + (t(objects1[i,] - mu1) %*% 
(objects1[i,] - mu1)) / (rows + 2)     
}          
for (i in 1:rows2)     
{         
sigma <- sigma + (t(objects2[i,] - mu2) %*% 
(objects2[i,] - mu2)) / (rows + 2)     
}          
return (sigma) 
}


 ## Оценивание 
objectsOfFirstClass <- xl[xl[,3] == 1, 1:2] 
objectsOfSecondClass <- xl[xl[,3] == 2, 1:2] 
mu1 <- estimateMu(objectsOfFirstClass) 
mu2 <- estimateMu(objectsOfSecondClass) 
 Sigma <- estimateFisherCovarianceMatrix(objectsOfFirstClass, objectsOfSecondClass, mu1, mu2) 
 
 
 ## Получаем коэффициенты ЛДФ 
inverseSigma <- solve(Sigma) alpha <- inverseSigma %*% t(mu1 - mu2) 
mu_st <- (mu1 + mu2) / 2 
beta <- mu_st %*% alpha 
```
  <p><img src= "лдф.png">


   Эвристика линейного дискриминанта Фишера является в некотором роде упрощением квадратичного дискриминанта. Она используется с целью получить более устойчивый алгоритм классификации. Наиболее целесообразно пользоваться линейным дискриминантом Фишера, когда данных для обучения недостаточно. Вследствие основной гипотезы, на которой базируется алгоритм, наиболее удачно им решаются простые задачи классификации, в которых по формам классы "похожи" друг на друга. 

# Линейные классиикаторы

  
# Метод стахостического градиента
Алгоритм    <p><img src= "алгоритмЛК.PNG"> является линейным алгоритм классификации. 
 
 Уравнение <𝑤,𝑥>=0 задаёт разделяющую классы гиперплоскость в пространстве ℝ.  Для подбора оптимального (минимизирующего эмпирический риск <p><img src= "ЭМПриск.PNG"> значения вектора весов 𝑤 будем пользоваться методом стохастического градиента — итерационный процесс, на каждом шаге которого сдвигаемся в сторону противоположную вектору градиента  <p><img src= "вектор.PNG">  до тех пор, пока вектор весов 𝑤 не перестанет изменяться, причем вычисления градиента производится не на всех объектах обучения, а выбирается случайный объект (отсюда и название метода «стохастический»), на основе которого и происходят вычисления. Требуется найти ветор параметра w, при котором достигается минимум аппроксимирваного эмпирического риска.
Применяем метод градиентного спуска. В этом методе выбирается некоторое начальное приближение для w, затем запускается интерационный процесс, на каждом шаге которого вектор w изменяется в направлении наиболее быстрого убывания функционала Q. В зависимости от функции потерь, которая используется в функционале эмпирического риска, будем получать различные линейные алгоритмы классификации. 
  
  ## Достоинства:
  * легко реализуется
  * применим к любым моделям и функциям потерь
  * допускает потоковое обучение
  * на сверхбольших выборках позволяет получать неплохие решения, даже не обработав все (х, у)
  ## Недостатки:
  * возможно застревание в локальных экстремумах 
  * возможна расходимость или медленная сходимость
  * возможно переобучение
  
  
  При использовании метода стохастического градиента необходимо нормализовать исходные данные: 
 ```R
## Нормализация обучающей выборки 
trainingSampleNormalization <- function(xl) 
{    
   n <- dim(xl)[2] - 1     
   for(i in 1:n)     
{         
    xl[, i] <- (xl[, i] - mean(xl[, i])) / sd(xl[, i])     
    }     
return (xl) 
} 
 
## Добавление колонки для из -1 для w0 
  trainingSamplePrepare <- function(xl) 
{     
    l <- dim(xl)[1]     
    n <- dim(xl)[2] - 1     
    xl <- cbind(xl[, 1:n], seq(from = -1, to = -1, length.out = l), xl[, n + 1]) 
} 
 ``` 
   Алгоритм классификации ADALINE — адаптивный линейный элемент, в качестве функции потерь используется квадратичная функция потерь:

```R 
      ## Квадратичная функция потерь 
        lossQuad <- function(x) 
     {     
       return ((x-1)^2) }  
  ```
  Персептрон Розенблатта — линейный классификатор, обучаемый с помощью стохастического градиента с правилом Хэбба и кусочно-линейной функции потерь:
 
 ```R
 ## Функция потерь для правила Хэбба 
    lossPerceptron <- function(x) 
{    
 return (max(-x, 0)) 
} 
  ## Стохастический градиент с правилом Хебба 
   sg.Hebb <- function(xl, eta = 0.1, lambda = 1/6) 
 {     
     l <- dim(xl)[1]     
     n <- dim(xl)[2] - 1       
     w <- c(1/2, 1/2, 1/2)     
   iterCount <- 0          
  ## initialize Q     
    Q <- 0     
     for (i in 1:l)     
 {         
  ## calculate the scalar product <w,x>        
     wx <- sum(w * xl[i, 1:n])           
  ## calculate a margin          
     margin <- wx * xl[i, n + 1]          
  # Q <- Q + lossQuad(margin)         
     Q <- Q + lossPerceptron(margin)     
 }          
  repeat     
 {
  ```
  
  Логистическая регрессия — линейный байесовский классификатор, использующий логарифмическую функцию потерь, имеет ряд интересных особенностей, например, алгоритм способен помимо определения принадлежности объекта к классу определять и степень его принадлежности. Является одним из популярных алгоритмом классификации. 
  ```R
  ## Логарифмическая функция потерь 
    lossLog <- function(x) 
  {     
    return (log2(1 + exp(-x))) } 
 ## Сигмоидная функция 
    sigmoidFunction <- function(z) 
 {     
   return (1 / (1 + exp(-z))) 
 } 
 ## Стохастический градиент для логистической регрессии 
 sg.LogRegression <- function(xl) 
 {     
      l <- dim(xl)[1]    
      n <- dim(xl)[2] - 1       
      w <- c(1/2, 1/2, 1/2)     
     iterCount <- 0     
     lambda <- 1/l         
   ## initialize Q     
     Q <- 0     
      for (i in 1:l)     
   {         
   ## calculate the scalar product <w,x>         
    wx <- sum(w * xl[i, 1:n])    
   ## calculate a margin          
     margin <- wx * xl[i, n + 1]                   
     Q <- Q + lossSigmoid(margin)        
   }          
     repeat     
   { 
  
  ```
  
  <p><img src= "ЛК.PNG">
  
# Проблема переобучения 
## Возможные причины переобучения: 
* линейная зависимость (мультиколлинеарность) признаков
* слишком мало объектов, слишком много признаков

## Проявление переобучения 
* слишком большие веса разных знаков 
* неустойчивость классификации а(х, w) относительно погрешностей измерения признаков

 В методе стахостического градиента необходимы различные эвристики для улучшения сходимости и получения лучшего решения. Регуляризация(сокращение весов) решает проблему мультиколлинеарности и снижает риск переобучения.
