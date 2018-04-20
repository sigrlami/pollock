import { fromJS, Map, List } from 'immutable';
import { IAction, IEditableQuestion } from '../../../shared/types/app';
import initial, { initialQuestion, initialAnswer } from './initial';
import { IReduxState } from '../namespace';

function reducer(state: IReduxState = initial, { type, payload }: IAction): IReduxState {
  const imState: Map<string, any> = fromJS(state);

  switch (type) {
  case 'CREATE_POLL:CHANGE_FIELD_VALUE': {
    interface IData { fieldName: string; fieldValue: string; }
    const data: IData = payload as IData;
    return imState
      .setIn(['newPoll', data.fieldName], data.fieldValue)
      .toJS();
  }
  case 'CREATE_POLL:ADD_ANSWER': {
    const index: number = payload as number;
    const questions: List<Map<string, any>> = imState.getIn(['newPoll', 'questions']);
    const question: Map<string, any> = questions.get(index);

    return imState
      .setIn(
        ['newPoll', 'questions'],
        questions.set(index, question.set('answers', question.get('answers').push(initialAnswer))),
      ).toJS();
  }
  case 'CREATE_POLL:ADD_QUESTION': {
    return imState.setIn(
      ['newPoll', 'questions'],
      (imState.getIn(['newPoll', 'questions']) as List<IEditableQuestion>).push(initialQuestion),
    ).toJS();
  }
  case 'CREATE_POLL:CHANGE_ANSWER_TEXT': {
    interface IData { questionIndex: number; answerIndex: number; value: string; }
    const data: IData = payload as IData;
    const questions = imState.getIn(['newPoll', 'questions']);
    const question = questions.get(data.questionIndex);
    const answers = question.get('answers');
    const answer = answers.get(data.answerIndex);

    return imState.withMutations((mutable: Map<string, any>) => mutable.setIn(
      ['newPoll', 'questions'],
      questions.set(
        data.questionIndex,
        question.set('answers', answers.set(data.answerIndex, answer.set('text', data.value))),
      ),
    )).toJS();
  }
  case 'CREATE_POLL:CHANGE_QUESTION_FIELD': {
    interface IData { questionIndex: number; value: string; fieldName: string; }
    const data = payload as IData;
    const questions = imState.getIn(['newPoll', 'questions']);
    return imState.setIn(
      ['newPoll', 'questions'],
      questions.set(
        data.questionIndex,
        questions.get(data.questionIndex).set(data.fieldName, data.value),
      ),
    ).toJS();
  }
  case 'CREATE_POLL:SEND_NEW_POLL_SUCCESS':
    return imState.set('newPoll', initial.newPoll).toJS();
  case 'CREATE_POLL:LOAD_POLL_SUCCESS':
    return imState.set('newPoll', payload).toJS();
  case 'CREATE_POLL:CLEAR_DATA':
    return imState.set('newPoll', initial.newPoll).toJS();
  default:
    return state;
  }
}

export default reducer;
