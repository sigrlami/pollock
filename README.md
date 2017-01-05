# Pollock

[EN](#en) | [RU](#ru) | [UA](#ua)

## How to run?

1. Download stack tool http://haskellstack.org/ for your platform 
2. Navigate to top directory
3. Run `stack build` command, which will download everything that's necessary for the project
4. Run `stack exec -- pollock -p 4100` to run web server with code
5. Install PostgreSQL server with db `pollock` and user `pollock`, you can see config add `snaplets/postgresql-simple/devel.cfg`


## En

`pollock` is a complete web application written in Haskell, intended as education project and reference for beginners. You can see development changes by looking at the git commit history of this repository, each stage represent small step in development. I start from simplistic "Hello, World!" app to a fully-functional social voting application with PostgreSQL database integration.

### Starting out

Begin by entering the following commands into your terminal window.  You'll need to have Haskell installed.

```sh
$ git clone git@github.com:sigrlami/pollock.git
$ cd pollock
$ git checkout stage-1
$ cabal install
$ ./dist/build/pollock/pollock
```

Now, open a browser and go to [http://localhost:8000/](http://localhost:8000/).

You should see "Hello, Pollock!" displayed.  If you look at [src/Main.hs in stage-1](), you can see that this is achieved with just a few simple lines of Haskell.

To shut down the pollock server, press Ctrl-C in your terminal window.  Once you've taken a look around in Stage 1, you can move on to Stage 2 by running

```sh
$ git checkout stage-2
$ cabal install
$ ./dist/build/pollock/pollock
```

### Stages

Although looking at the checked out code may be helpful, a more informative way to look at this project is by viewing the commit history.  Each stage is a single git commit, designed to add a single piece of functionality, as you might do if you were creating a website from scratch.

You can take a look at the changes made in each stage here:

* [Stage 1]()
* [Stage 2]()
* [Stage 3]()
* [Stage 4]()
* [Stage 5]()
* [Stage 6]()
* [Stage 7]()

Note: You need PostgreSQL installed for all following stages.

* [Stage 8]()
* [Stage 9]()
* [Stage 10]()
* [Stage 11]()
* [Stage 12]()
* [Stage 13]()
* [Stage 14]()

## Ru
`pollock` это полноценное веб приложение написанное на Haskell, предназначенное в качестве проекта для обучения и справки для начинающих. Вы можете посмотреть изменения в процессе разработки с помощью истории git коммитов, каждый этап представляет небольшой шаг в процессе разработки. Я начинаю с простейшего "Hello, World!" приложения и заканчиваю полностью рабочим приложения обеспечивающим интеграцию с базой данных PostgreSQL, приложения для онлайн голосования.

### Начинаем

Для того, чтоб начать введите следующие команды в окне терминала. У вас должне быть установлен Haskell.

```sh
$ git clone git@github.com:sigrlami/pollock.git
$ cd pollock
$ git checkout stage-1
$ cabal install
$ ./dist/build/pollock/pollock
```
Теперь откройте обозреватель и перейдите по адресу [http://localhost:8000/](http://localhost:8000/).

Вы должны будете увидеть надпись "Hello, Pollock!". Если вы посмотрите [src/Main.hs в этапе-1/stage-1](), вы увидете насколько просто можно это сделать, всего несколькими строками кода.

Для того, чтоб отключить сервер `pollock`, нажмите CTRL-C в вашем терминале. После того, как вы просмотрите изменения Этапа 1, вы можете переходить к Этапу 2.

```sh
$ git checkout stage-2
$ cabal install
$ ./dist/build/pollock/pollock
```

### Этапы

Несмотря на то, что просмотр уже завершенного этапа полезен, более информативный способ для изучения это просмотр истории коммитов каждого бранча. Каждый этап это отдельный бранч, с набором коммитов, реализующие кусочек функциональности, так как будто вы создаете приложения с нуля.

Вы можете просмотреть изменения сделанные на каждом этапе по ссылкам:

* [Stage 1]()
* [Stage 2]()
* [Stage 3]()
* [Stage 4]()
* [Stage 5]()
* [Stage 6]()
* [Stage 7]()

Примечание: У вас должен быть установлен PostgreSQL для работы со следующими этапами

* [Stage 8]()
* [Stage 9]()
* [Stage 10]()
* [Stage 11]()
* [Stage 12]()
* [Stage 13]()
* [Stage 14]()


## Ua

`pollock` це повноцінний веб додаток написаний на Haskell, призначений у якості проекта для навчання та довідки для початківців. Ви можете продивитися зміни у процессі розробки з допомогою git коммітів, кожний етап це невеличкий крок у процессі розробки. Я починаю з найпростішого "Hello, World!" додатка та закінчую повністю робочим додатком, забезпечуючим інтеграцію с базою данних PostgreSQL, додаток для онлайн голосування.

### Починаемо

Для того, шоб почати введіть наступні команди в вікні термінала. У вас має бути встановлен Haskell.

```sh
$ git clone git@github.com:sigrlami/pollock.git
$ cd pollock
$ git checkout stage-1
$ cabal install
$ ./dist/build/pollock/pollock
```

Тепер відкрийте оглядач та перейдіть за адресою [http://localhost:8000/](http://localhost:8000/).

Ви маєте побачити напис "Hello, Pollock!". Якщо ви подивитесь [src/Main.hs в этапе-1/stage-1](), то побачите наскыльки легко можно це зробити, лише декіклька строк коду.

Для того, чтоб отключить сервер `pollock`, нажмите CTRL-C в вашем терминале. После того, как вы просмотрите изменения Этапа 1, вы можете переходить к Этапу 2.

Для того, щоб відключити сервер , натисніть у вашому терміналі. Після того, як ви продивитесь зміни Етапу 1, ви можете переходити до Етапу 2.

```sh
$ git checkout stage-2
$ cabal install
$ ./dist/build/pollock/pollock
```

### Етапи

Несмотря на то, что просмотр уже завершенного этапа полезен, более информативный способ для изучения это просмотр истории коммитов каждого бранча. Каждый этап это отдельный бранч, с набором коммитов, реализующие кусочек функциональности, так как будто вы создаете приложения с нуля.

Не дивлячись на те, що перегляд вже закінченного етапу корисен, більш інформативний спосіб для вивчення це перегляд історії комітів кожного бранчу. Кожний етап це окремий бранч, з набором комітів, які реалізують шматочок функціоналу, так якщо б ви створювали додаток з нуля.

Ви можете продивитися зміни зроблени на кожному етпі по ссилкам:

* [Stage 1]()
* [Stage 2]()
* [Stage 3]()
* [Stage 4]()
* [Stage 5]()
* [Stage 6]()
* [Stage 7]()

Примітка: У вас має бути встановлений PostgreSQL для роботы з наступними етапами

* [Stage 8]()
* [Stage 9]()
* [Stage 10]()
* [Stage 11]()
* [Stage 12]()
* [Stage 13]()
* [Stage 14]()
