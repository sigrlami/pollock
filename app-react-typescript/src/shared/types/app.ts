import { ReactElement } from 'react';
import { Route, RouterOnContext } from 'react-router';
import { Store } from 'redux';
import { ThunkAction, ActionCreator } from 'redux';
import { Reducer } from 'redux';
import { namespace as createPollNamespace } from 'features/editPoll';
import { namespace as viewPollsNamespace } from 'features/viewPolls';
import Api from '../api/Api';

interface IModule<S> {
  router: RouterOnContext | null;
  store: Store<IReduxState> | null;
  getRoutes?: () => ReactElement<Route.RouteProps> | Array<ReactElement<Route.RouteProps>>;
  getReducer?: () => { name: string; reducer: Reducer<S> };
}

interface IExtraArguments {
  api: Api;
}

interface IAction {
  payload?: { [key: string]: any } | number | string | null;
  type: string;
}

interface IReduxState {
  createPoll: createPollNamespace.IReduxState;
  viewPolls: viewPollsNamespace.IReduxState;
}

interface ICommunication {
  isRequesting: boolean;
  error: string;
}

type AsyncActionCreatorResult = ThunkAction<Promise<void>, IReduxState, IExtraArguments>;
type AsyncActionCreator = ActionCreator<AsyncActionCreatorResult>;
type QuestionType = 'YesNo' | 'SingleChoice';

/* Main app entities */
interface IEditablePoll {
  id?: string;
  isActive: boolean;
  title: string;
  description: string;
  startTime: string;
  endTime: string;
  questions: IEditableQuestion[];
}

interface IEditableQuestion {
  type: 'YesNo' | 'SingleChoice';
  text: string;
  answers: IEditableAnswer[];
}

interface IEditableAnswer {
  id?: string;
  text: string;
}

interface IPoll {
  id: string | number;
  isActive: boolean;
  title: string;
  description: string;
  startTime: string;
  endTime: string;
  questions: IQuestion[];
}

interface IQuestion {
  type: 'YesNo' | 'SingleChoice';
  text: string;
  answers: IAnswer[];
}

interface IAnswer {
  id: string;
  text: string;
  isSelected: boolean;
}

interface IPollResult {
  id?: string;
  pollId: string;
  questions: IQuestionResult[];
}

interface IQuestionResult {
  answerId: string;
  text: string;
  answer: string;
}

export {
  IQuestionResult,
  IPollResult,
  ICommunication,
  IQuestion,
  IAnswer,
  IPoll,
  QuestionType,
  IEditablePoll,
  IEditableQuestion,
  IEditableAnswer,
  IModule,
  IAction,
  IExtraArguments,
  IReduxState,
  AsyncActionCreator,
  AsyncActionCreatorResult
};
