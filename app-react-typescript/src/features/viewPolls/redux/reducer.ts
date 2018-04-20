import { fromJS, Map } from 'immutable';
import { IReduxState } from '../namespace';
import { IAction, IPollResult, IPoll } from 'shared/types/app';
import initial from './initial';

function reducer(state: IReduxState = initial, { type, payload }: IAction): IReduxState {
  const imState: Map<string, any> = fromJS(state);
  switch (type) {
  case 'VIEW_POLLS:LOAD_POLLS_SUCCESS':
    return imState.set('polls', payload).toJS();
  case 'VIEW_POLLS:LOAD_POLL_SUCCESS': {
    interface IData { results: IPollResult[]; poll: IPoll; }
    const data = payload as IData;
    return imState.set('selectedPoll', data).toJS();
  }
  case 'VIEW_POLLS:SELECT_ANSWER': {
    interface IData { questionIndex: number; answerIndex: number; }
    const data = payload as IData;
    const questions = imState.getIn(['selectedPoll', 'poll', 'questions']);
    const question = questions.get(data.questionIndex);

    return imState.setIn(
      ['selectedPoll', 'poll', 'questions', data.questionIndex],
      question.set('answers', question.get('answers').map(
          (answer: Map<string, any>, index: number) => answer.set('isSelected', index === data.answerIndex),
      )),
    ).toJS();
  }
  default: return state;
  }
}

export default reducer;
