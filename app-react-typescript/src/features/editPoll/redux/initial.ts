import { IReduxState } from '../namespace';
import { IEditableQuestion, IEditableAnswer } from '../../../shared/types/app';

const initial: IReduxState = {
  newPoll: {
    title: '',
    isActive: true,
    description: '',
    endTime: '',
    startTime: '',
    questions: [],
  },
  communication: {
    pollCreating: {
      isRequesting: false,
      error: '',
    },
  },
  questionsTemplates: [
    { text: 'Lorem ipsum dolor sit amet', answers: ['Integer scelerisque posuere', 'quam blandit cursus'] },
    { text: 'Suspendisse vel bibendum mi', answers: ['Pellentesque eu', 'lorem a tellus', 'bibendum id et nibh'] },
    { text: 'Mauris ac ultrices libero', answers: ['Maecenas condimentum', 'felis accumsan'] },
    { text: 'Aenean convallis viverra augue', answers: ['arcu sodales'] },
    { text: 'sed tempor mi rutrum id.', answers: ['nec varius enim', 'Ut vitae eleifend odio'] },
    { text: 'Lorem ipsum dolor sit amet', answers: ['vel tristique urna'] },
  ],
};

const initialQuestion: IEditableQuestion = {
  type: 'SingleChoice',
  text: '',
  answers: [],
};

const initialAnswer: IEditableAnswer = {
  text: '',
};

export { initialQuestion, initialAnswer };
export default initial;
